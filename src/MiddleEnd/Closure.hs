{-# LANGUAGE GADTs #-}

module MiddleEnd.Closure (getFunctions, ClosureEnv (ClosureEnv)) where

import Control.Monad (unless)
import Control.Monad.State (MonadTrans (lift), State, StateT (runStateT), execState, get, gets, modify)
import MiddleEnd.Analysis.Identifier (IdentEnvT, genNewVar, getTyOf, identState)
import Syntax
import Typing (TypeBase (TUnit))

newtype ClosureEnv = ClosureEnv
    { functions :: [Function]
    }

type ClosureT m = StateT ClosureEnv (IdentEnvT m)

addFunction :: (Monad m) => Function -> ClosureT m ()
addFunction f = do
    modify $ \env -> env{functions = f : functions env}

findFunction :: (Monad m) => Ident -> ClosureT m (Maybe Function)
findFunction ident =
    gets (findFunction' ident . functions)
  where
    findFunction' _ [] = Nothing
    findFunction' ident' (f : fs)
        | funcName f == ident' = Just f
        | otherwise = findFunction' ident' fs

updateFunctionExpr :: (Monad m) => Ident -> ClosureExpr -> ClosureT m ()
updateFunctionExpr ident expr = do
    funcList <- gets functions
    modify $ \env -> env{functions = updateFunctionExpr' funcList}
  where
    updateFunctionExpr' :: [Function] -> [Function]
    updateFunctionExpr' [] = []
    updateFunctionExpr' (func : rest)
        | funcName func == ident = func{funcBody = expr} : rest
        | otherwise = func : updateFunctionExpr' rest

data FreeArgsEnv = FreeArgsEnv
    { eFreeVars :: [Ident]
    , eBoundedVars :: [Ident]
    }

registerFreeVar :: Ident -> State FreeArgsEnv ()
registerFreeVar (ExternalIdent _) =
    -- External identifiers are not free variables.
    pure ()
registerFreeVar ident = do
    env <- get
    unless (ident `elem` eBoundedVars env) $
        unless (ident `elem` eFreeVars env) $
            modify $
                \env' -> env'{eFreeVars = ident : eFreeVars env}

addBounded :: Ident -> State FreeArgsEnv ()
addBounded ident = modify $ \env -> env{eBoundedVars = ident : eBoundedVars env}

getFreeVars :: KExpr -> [Ident] -> [Ident]
getFreeVars kExpr bounded = eFreeVars $ execState (getFreeVarState kExpr) (FreeArgsEnv [] bounded)
  where
    getFreeVarState :: KExpr -> State FreeArgsEnv ()
    getFreeVarState (Const _ _) = pure ()
    getFreeVarState (Unary _ _ operand) = registerFreeVar operand
    getFreeVarState (Binary _ _ operand1 operand2) = do
        registerFreeVar operand1
        registerFreeVar operand2
    getFreeVarState (If _ (CIdentity cond) thenExpr elseExpr) = do
        registerFreeVar cond
        getFreeVarState thenExpr
        getFreeVarState elseExpr
    getFreeVarState (If _ (CNeg cond) thenExpr elseExpr) = do
        registerFreeVar cond
        getFreeVarState thenExpr
        getFreeVarState elseExpr
    getFreeVarState (If _ (CComp _ lhs rhs) thenExpr elseExpr) = do
        registerFreeVar lhs
        registerFreeVar rhs
        getFreeVarState thenExpr
        getFreeVarState elseExpr
    getFreeVarState (Let _ PUnit expr body) = do
        getFreeVarState expr
        getFreeVarState body
    getFreeVarState (Let _ (PVar v) expr body) = do
        -- Since the identifiers are unique, we don't have to care about shadowing.
        addBounded v
        getFreeVarState expr
        getFreeVarState body
    getFreeVarState (Let _ (PTuple vs) expr body) = do
        mapM_ addBounded vs
        getFreeVarState expr
        getFreeVarState body
    getFreeVarState (Let _ (PRec func args) expr body) = do
        addBounded func
        mapM_ addBounded args
        getFreeVarState expr
        getFreeVarState body
    getFreeVarState (Var _ v) = do
        registerFreeVar v
    getFreeVarState (Tuple _ values) = do
        mapM_ registerFreeVar values
    getFreeVarState (ArrayCreate _ size value) = do
        registerFreeVar size
        registerFreeVar value
    getFreeVarState (Get _ array index) = do
        registerFreeVar array
        registerFreeVar index
    getFreeVarState (Put _ array index value) = do
        registerFreeVar array
        registerFreeVar index
        registerFreeVar value
    getFreeVarState (App _ func args) = do
        registerFreeVar func
        mapM_ registerFreeVar args
    getFreeVarState (Loop _ args values body) = do
        mapM_ addBounded args
        mapM_ registerFreeVar values
        getFreeVarState body
    getFreeVarState (Continue _ values) = do
        mapM_ registerFreeVar values

-- | Determines whether the function has been treated as a closure.
isUsedAsClosure :: Ident -> KExpr -> Bool
isUsedAsClosure ident (If _ _ thenExpr elseExpr) =
    isUsedAsClosure ident thenExpr || isUsedAsClosure ident elseExpr
isUsedAsClosure ident (Let _ _ expr body) =
    isUsedAsClosure ident expr || isUsedAsClosure ident body
isUsedAsClosure ident (Var _ ident') =
    ident == ident'
isUsedAsClosure ident (App _ _ args) =
    ident `elem` args
isUsedAsClosure ident (Tuple _ values) =
    ident `elem` values
isUsedAsClosure ident (ArrayCreate _ _ value) =
    ident == value
isUsedAsClosure ident (Put _ _ _ value) =
    ident == value
isUsedAsClosure _ _ = False

-- | Determines whether the function is used.
isUsed :: Ident -> KExpr -> Bool
isUsed ident (If _ _ thenExpr elseExpr) =
    isUsed ident thenExpr || isUsed ident elseExpr
isUsed ident (Let _ _ expr body) =
    isUsed ident expr || isUsed ident body
isUsed ident (Var _ ident') =
    ident == ident'
isUsed ident (App _ func args) =
    func == ident || ident `elem` args
isUsed ident (Tuple _ values) =
    ident `elem` values
isUsed ident (ArrayCreate _ _ value) =
    ident == value
isUsed ident (Put _ _ _ value) =
    ident == value
isUsed _ _ = False

getFunctions :: (Monad m) => KExpr -> IdentEnvT m [Function]
getFunctions expr = do
    (expr', funcList) <- runStateT (genFunctions expr) (ClosureEnv [])
    pure $ Function (getExprState expr') True (Entry dummyLoc) [] [] expr' : functions funcList

dummyState :: PTypedState
dummyState = TState TUnit dummyLoc

dummyExpr :: ClosureExpr
dummyExpr = Const dummyState LUnit

genFunctions :: (Monad m) => KExpr -> ClosureT m ClosureExpr
genFunctions (Let state (PRec func args) expr body) = do
    funcState <- lift $ identState func
    reducedFreeVars <- reduceFree freeVars'
    if null reducedFreeVars && not isUsedAsClosure'
        then do
            -- No free variables and no usage as closure means we don't have to create a closure.
            addFunction (Function funcState True func [] args dummyExpr)
            expr' <- genFunctions expr
            updateFunctionExpr func expr'
            genFunctions body
        else do
            newFreeVars <-
                lift $
                    mapM
                        ( \ident -> do
                            ty <- getTyOf ident
                            genNewVar ty
                        )
                        reducedFreeVars
            addFunction (Function funcState False func newFreeVars args dummyExpr)
            let replacedExpr =
                    foldl
                        ( \expr' (ident, newIdent) ->
                            subst
                                ident
                                newIdent
                                expr'
                        )
                        expr
                        (zip reducedFreeVars newFreeVars)
            expr' <- genFunctions replacedExpr
            if isUsed func expr
                then do
                    -- The function is a recursive function with free variables.
                    -- Create a closure inside it to avoid referencing out-of-scope closures.
                    funcTy <- lift $ getTyOf func
                    newFunc <- lift $ genNewVar funcTy
                    let replacedExpr' =
                            subst
                                func
                                newFunc
                                expr'
                    updateFunctionExpr func $
                        Let (getExprState expr) (PVar newFunc) (MakeClosure funcState func newFreeVars) replacedExpr'
                else
                    updateFunctionExpr func expr'
            body' <- genFunctions body
            funcTy <- lift $ getTyOf func
            newFunc <- lift $ genNewVar funcTy
            let replacedBody =
                    subst
                        func
                        newFunc
                        body'
            pure $ Let state (PVar newFunc) (MakeClosure funcState func reducedFreeVars) replacedBody
  where
    freeVars' = getFreeVars expr (func : args)
    isUsedAsClosure' = isUsedAsClosure func expr || isUsedAsClosure func body

    reduceFree :: (Monad m) => [Ident] -> ClosureT m [Ident]
    reduceFree [] = pure []
    reduceFree (var : rest) = do
        found <- findFunction var
        rest' <- reduceFree rest
        case found of
            Just found' ->
                pure $ if isDirect found' then rest' else var : rest'
            Nothing ->
                pure $ var : rest'
genFunctions (App state func args) = do
    func' <- findFunction func
    case (func', func) of
        (Just f, _) -> do
            if isDirect f
                then do
                    pure $ DirectApp state func args
                else do
                    pure $ ClosureApp state func args
        (Nothing, ExternalIdent _) -> do
            -- Calling an external function.
            pure $ DirectApp state func args
        (Nothing, _) -> do
            -- Calling a closure.
            pure $ ClosureApp state func args
genFunctions (Const state lit) =
    pure $ Const state lit
genFunctions (Unary state op operand) =
    pure $ Unary state op operand
genFunctions (Binary state op operand1 operand2) =
    pure $ Binary state op operand1 operand2
genFunctions (If state cond thenExpr elseExpr) = do
    thenExpr' <- genFunctions thenExpr
    elseExpr' <- genFunctions elseExpr
    pure $ If state cond thenExpr' elseExpr'
genFunctions (Let state PUnit expr body) = do
    expr' <- genFunctions expr
    body' <- genFunctions body
    pure $ Let state PUnit expr' body'
genFunctions (Let state (PVar v) expr body) = do
    expr' <- genFunctions expr
    body' <- genFunctions body
    pure $ Let state (PVar v) expr' body'
genFunctions (Let state (PTuple vs) expr body) = do
    expr' <- genFunctions expr
    body' <- genFunctions body
    pure $ Let state (PTuple vs) expr' body'
genFunctions (Var state ident) = pure $ Var state ident
genFunctions (Tuple state values) =
    pure $ Tuple state values
genFunctions (ArrayCreate state size value) =
    pure $ ArrayCreate state size value
genFunctions (Get state array index) =
    pure $ Get state array index
genFunctions (Put state array index value) =
    pure $ Put state array index value
genFunctions (Loop state args values body) = do
    body' <- genFunctions body
    pure $ Loop state args values body'
genFunctions (Continue state values) =
    pure $ Continue state values
