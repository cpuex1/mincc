{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.TypeInferrer (
    genNewId,
    defaultEnv,
    inferType,
    TypeEnv (TypeEnv, assigned, table, variables),
    registerIdent,
    registerAll,
    removeVar,
) where

import Builtin (BuiltinFunction (builtinName, builtinType), builtinFunctions)
import Control.Monad (void)
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.State
import Data.Map (Map, fromList, insert, lookup)
import Display
import Error (CompilerError (TypeError))
import Syntax
import Typing
import Prelude hiding (lookup)

newtype ITypedExpr = ITGuard {iTExp :: Expr (ITy, Loc) Ident ITypedExpr DisallowClosure ()}
    deriving (Show, Eq)

getTy :: ITypedExpr -> ITy
getTy = fst . getExprState . iTExp

data TypeEnv = TypeEnv
    { assigned :: Int
    , table :: Map TypeId ITy
    , variables :: Map Ident TypeId
    }
    deriving (Show, Eq)

type TypingM = ExceptT CompilerError (State TypeEnv)

defaultEnv :: TypeEnv
defaultEnv =
    TypeEnv builtinLength builtinVarsTable builtinVars
  where
    builtinVars = fromList $ zip (map builtinName builtinFunctions) ([0 ..] :: [TypeId])
    builtinVarsTable = fromList $ zip ([0 ..] :: [TypeId]) (map (weakenTy . builtinType) builtinFunctions)
    builtinLength = length builtinFunctions

genNewId :: TypingM TypeId
genNewId = do
    a <- gets assigned
    modify (\e -> e{assigned = assigned e + 1})
    pure a

fromTypeId :: TypeId -> TypingM (Maybe ITy)
fromTypeId tId =
    gets (lookup tId . table)

registerIdent :: Ident -> TypingM ()
registerIdent ident = do
    env <- get
    case lookup ident $ variables env of
        Just _ -> pure ()
        Nothing -> do
            tId <- genNewId
            modify (\e -> e{variables = insert ident tId $ variables e})

registerAll :: ResolvedExpr -> TypingM ()
registerAll (RGuard expr) =
    void $ visitExprM pure (\ident -> registerIdent ident >> pure ident) registerAll expr

getTyOfIdent :: Ident -> TypingM TypeId
getTyOfIdent ident = do
    env <- get
    case lookup ident $ variables env of
        Just tId ->
            pure tId
        Nothing -> throwError $ TypeError (Just $ identLoc ident) "Detected an identifier not tracked"

updateEnv :: TypeId -> ITy -> TypingM ()
updateEnv typeId ty = do
    table' <- gets (insert typeId ty . table)
    resolved <- mapM resolveTy table'
    modify (\e -> e{table = resolved})

-- | Remove all type variables from a type.
removeVar :: TypeEnv -> ITy -> Ty
removeVar env ty =
    evalState
        ( do
            result <- runExceptT $ applyEnv ty
            case result of
                Left _ -> error "panic!"
                Right ty' -> pure ty'
        )
        env

resolveTy :: ITy -> TypingM ITy
resolveTy TUnit = pure TUnit
resolveTy TBool = pure TBool
resolveTy TInt = pure TInt
resolveTy TFloat = pure TFloat
resolveTy (TFun args ret) = TFun <$> mapM resolveTy args <*> resolveTy ret
resolveTy (TTuple values) = TTuple <$> mapM resolveTy values
resolveTy (TArray value) = TArray <$> resolveTy value
resolveTy (TVar tId) = do
    t <- fromTypeId tId
    case t of
        Just t' -> resolveTy t'
        Nothing -> pure $ TVar tId

applyEnv :: ITy -> TypingM Ty
applyEnv TUnit = pure TUnit
applyEnv TBool = pure TBool
applyEnv TInt = pure TInt
applyEnv TFloat = pure TFloat
applyEnv (TFun args ret) = TFun <$> mapM applyEnv args <*> applyEnv ret
applyEnv (TTuple values) = TTuple <$> mapM applyEnv values
applyEnv (TArray value) = TArray <$> applyEnv value
applyEnv (TVar tId) = do
    t <- fromTypeId tId
    case t of
        Just t' -> applyEnv t'
        Nothing -> do
            updateEnv tId TInt
            pure TInt

applyEnvE :: ITypedExpr -> TypingM TypedExpr
applyEnvE (ITGuard expr) = do
    expr' <- visitExprM fState pure applyEnvE expr
    return $ TGuard expr'
  where
    fState :: (ITy, Loc) -> TypingM TypedState
    fState (ty, pos) = applyEnv ty >>= \ty' -> return (TypedState ty' pos)

occurCheck :: TypeId -> ITy -> TypingM ()
occurCheck _ TUnit = pure ()
occurCheck _ TBool = pure ()
occurCheck _ TInt = pure ()
occurCheck _ TFloat = pure ()
occurCheck tId (TFun args ret) = do
    mapM_ (occurCheck tId) args
    occurCheck tId ret
occurCheck tId (TTuple values) = mapM_ (occurCheck tId) values
occurCheck tId (TArray value) = occurCheck tId value
occurCheck tId (TVar tId') = do
    if tId == tId'
        then throwError $ TypeError Nothing "Recursive type detected"
        else do
            ty <- fromTypeId tId'
            mapM_ (occurCheck tId) ty

unifyList :: [ITy] -> [ITy] -> TypingM ()
unifyList [] [] = pure ()
unifyList [] _ = throwError $ TypeError Nothing "Expected more arguments"
unifyList _ [] = throwError $ TypeError Nothing "Expected more arguments"
unifyList (x : xs) (y : ys) = unify x y >> unifyList xs ys

unify :: ITy -> ITy -> TypingM ()
unify TUnit TUnit = pure ()
unify TBool TBool = pure ()
unify TInt TInt = pure ()
unify TFloat TFloat = pure ()
unify (TFun args1 ret1) (TFun args2 ret2) = do
    unifyList args1 args2
    unify ret1 ret2
unify (TTuple values1) (TTuple values2) = unifyList values1 values2
unify (TArray value1) (TArray value2) = unify value1 value2
unify (TVar tId) rTy = do
    rTy' <- resolveTy rTy
    if rTy' == TVar tId
        then
            pure ()
        else do
            lTy <- resolveTy (TVar tId)
            if lTy == TVar tId
                then do
                    -- The type variable is not yet assigned.
                    occurCheck tId rTy'
                    updateEnv tId rTy'
                else
                    unify lTy rTy'
unify lTy (TVar tId) = unify (TVar tId) lTy
unify _ _ = throwError $ TypeError Nothing "Type mismatch"

doUnify :: Loc -> ITy -> ITy -> TypingM ()
doUnify pos ty1 ty2 = do
    catchError (unify ty1 ty2) $
        const $ do
            expected <- resolveTy ty1
            actual <- resolveTy ty2
            throwError $ TypeError (Just pos) $ "Expected type " <> display expected <> " but got " <> display actual

inferType :: ResolvedExpr -> (Either CompilerError TypedExpr, TypeEnv)
inferType expr = runState (runExceptT $ inferE expr) defaultEnv

inferE :: ResolvedExpr -> TypingM TypedExpr
inferE expr = do
    registerAll expr
    expr' <- inferIE expr
    case getTy expr' of
        TVar tId -> do
            doUnify (getExprState $ rExp expr) TUnit (TVar tId)
            applyEnvE expr'
        TUnit ->
            applyEnvE expr'
        _ ->
            throwError $
                TypeError (Just $ snd $ getExprState $ iTExp expr') $
                    "A main expression must return unit, not " <> display (getTy expr')

inferIE :: ResolvedExpr -> TypingM ITypedExpr
inferIE (RGuard (Const pos LUnit)) = pure $ ITGuard $ Const (TUnit, pos) LUnit
inferIE (RGuard (Const pos (LBool b))) = pure $ ITGuard $ Const (TBool, pos) (LBool b)
inferIE (RGuard (Const pos (LInt i))) = pure $ ITGuard $ Const (TInt, pos) (LInt i)
inferIE (RGuard (Const pos (LFloat f))) = pure $ ITGuard $ Const (TFloat, pos) (LFloat f)
inferIE (RGuard (Unary pos Not expr)) = do
    expr' <- inferIE expr
    doUnify (getExprState $ rExp expr) TBool $ getTy expr'
    pure $ ITGuard $ Unary (TBool, pos) Not expr'
inferIE (RGuard (Unary pos Neg (RGuard (Const _ (LFloat f))))) =
    -- Negative float literals
    pure $ ITGuard $ Const (TFloat, pos) (LFloat (-f))
inferIE (RGuard (Unary pos Neg expr)) = do
    expr' <- inferIE expr
    doUnify (getExprState $ rExp expr) TInt $ getTy expr'
    pure $ ITGuard $ Unary (TInt, pos) Neg expr'
inferIE (RGuard (Unary pos FNeg expr)) = do
    expr' <- inferIE expr
    doUnify (getExprState $ rExp expr) TFloat $ getTy expr'
    pure $ ITGuard $ Unary (TFloat, pos) FNeg expr'
inferIE (RGuard (Binary pos (RelationOp op) expr1 expr2)) = do
    expr1' <- inferIE expr1
    expr2' <- inferIE expr2
    doUnify (getExprState $ rExp expr2) (getTy expr1') (getTy expr2')
    pure $ ITGuard $ Binary (TBool, pos) (RelationOp op) expr1' expr2'
inferIE (RGuard (Binary pos (IntOp op) expr1 expr2)) = do
    expr1' <- inferIE expr1
    expr2' <- inferIE expr2
    doUnify (getExprState $ rExp expr1) TInt (getTy expr1')
    doUnify (getExprState $ rExp expr2) TInt (getTy expr2')
    pure $ ITGuard $ Binary (TInt, pos) (IntOp op) expr1' expr2'
inferIE (RGuard (Binary pos (FloatOp op) expr1 expr2)) = do
    expr1' <- inferIE expr1
    expr2' <- inferIE expr2
    doUnify (getExprState $ rExp expr1) TFloat (getTy expr1')
    doUnify (getExprState $ rExp expr2) TFloat (getTy expr2')
    pure $ ITGuard $ Binary (TFloat, pos) (FloatOp op) expr1' expr2'
inferIE (RGuard (If pos (CIdentity cond) thenExpr elseExpr)) = do
    cond' <- inferIE cond
    doUnify (getExprState $ rExp cond) TBool (getTy cond')
    thenExpr' <- inferIE (RGuard thenExpr)
    elseExpr' <- inferIE (RGuard elseExpr)
    doUnify (getExprState elseExpr) (getTy thenExpr') (getTy elseExpr')
    pure $ ITGuard $ If (getTy thenExpr', pos) (CIdentity cond') (iTExp thenExpr') (iTExp elseExpr')
inferIE (RGuard (Let pos PUnit expr body)) = do
    expr' <- inferIE (RGuard expr)
    body' <- inferIE (RGuard body)
    doUnify (getExprState expr) TUnit (getTy expr')
    pure $ ITGuard $ Let (getTy body', pos) PUnit (iTExp expr') (iTExp body')
inferIE (RGuard (Let pos (PVar v) expr body)) = do
    expr' <- inferIE (RGuard expr)
    vId <- getTyOfIdent v
    doUnify (getExprState expr) (TVar vId) (getTy expr')
    body' <- inferIE (RGuard body)
    pure $ ITGuard $ Let (getTy body', pos) (PVar v) (iTExp expr') (iTExp body')
inferIE (RGuard (Let pos (PRec func args) expr body)) = do
    expr' <- inferIE (RGuard expr)
    funcId <- getTyOfIdent func
    argIds <- mapM getTyOfIdent args
    doUnify (getExprState expr) (TVar funcId) (TFun (map TVar argIds) (getTy expr'))
    body' <- inferIE (RGuard body)
    pure $ ITGuard $ Let (getTy body', pos) (PRec func args) (iTExp expr') (iTExp body')
inferIE (RGuard (Let pos (PTuple values) expr body)) = do
    expr' <- inferIE (RGuard expr)
    valueIds <- mapM getTyOfIdent values
    doUnify (getExprState expr) (TTuple $ map TVar valueIds) (getTy expr')
    body' <- inferIE (RGuard body)
    pure $ ITGuard $ Let (getTy body', pos) (PTuple values) (iTExp expr') (iTExp body')
inferIE (RGuard (Var pos v)) = do
    vId <- getTyOfIdent v
    pure $ ITGuard $ Var (TVar vId, pos) v
inferIE (RGuard (App pos func args)) = do
    func' <- inferIE func
    args' <- mapM inferIE args
    retId <- genNewId
    doUnify (getExprState $ rExp func) (TFun (map getTy args') (TVar retId)) (getTy func')
    pure $ ITGuard $ App (TVar retId, pos) func' args'
inferIE (RGuard (Tuple pos values)) = do
    values' <- mapM inferIE values
    pure $ ITGuard $ Tuple (TTuple $ map getTy values', pos) values'
inferIE (RGuard (ArrayCreate pos size initVal)) = do
    size' <- inferIE size
    doUnify (getExprState $ rExp size) TInt (getTy size')
    initVal' <- inferIE initVal
    pure $ ITGuard $ ArrayCreate (TArray $ getTy initVal', pos) size' initVal'
inferIE (RGuard (Get pos array idx)) = do
    array' <- inferIE array
    idx' <- inferIE idx
    retId <- genNewId
    doUnify (getExprState $ rExp array) (TArray $ TVar retId) (getTy array')
    doUnify (getExprState $ rExp idx) TInt (getTy idx')
    pure $ ITGuard $ Get (TVar retId, pos) array' idx'
inferIE (RGuard (Put pos array idx value)) = do
    array' <- inferIE array
    idx' <- inferIE idx
    value' <- inferIE value
    doUnify (getExprState $ rExp array) (TArray $ getTy value') (getTy array')
    doUnify (getExprState $ rExp idx) TInt (getTy idx')
    pure $ ITGuard $ Put (TUnit, pos) array' idx' value'
