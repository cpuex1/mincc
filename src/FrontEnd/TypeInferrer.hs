{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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

data ITypedExprKind

instance ExprKind ITypedExprKind where
    type StateTy ITypedExprKind = (ITy, Loc)
    type IdentTy ITypedExprKind = Ident
    type OperandTy ITypedExprKind = ITypedExpr
    type ClosureTy ITypedExprKind = False
    type BranchTy ITypedExprKind = False

type ITypedExpr = Expr ITypedExprKind

getTy :: ITypedExpr -> ITy
getTy = fst . getExprState

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
registerAll = void . registerAll'
  where
    registerAll' :: ResolvedExpr -> TypingM ResolvedExpr
    registerAll' = visitExprM pure (\ident -> registerIdent ident >> pure ident) registerAll'

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
applyEnvE expr = do
    visitExprM fState pure applyEnvE expr
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
            doUnify (getExprState expr) TUnit (TVar tId)
            applyEnvE expr'
        TUnit ->
            applyEnvE expr'
        _ ->
            throwError $
                TypeError (Just $ snd $ getExprState expr') $
                    "A main expression must return unit, not " <> display (getTy expr')

inferIE :: ResolvedExpr -> TypingM ITypedExpr
inferIE (Const pos LUnit) = pure $ Const (TUnit, pos) LUnit
inferIE (Const pos (LBool b)) = pure $ Const (TBool, pos) (LBool b)
inferIE (Const pos (LInt i)) = pure $ Const (TInt, pos) (LInt i)
inferIE (Const pos (LFloat f)) = pure $ Const (TFloat, pos) (LFloat f)
inferIE (Unary pos Not expr) = do
    expr' <- inferIE expr
    doUnify (getExprState expr) TBool $ getTy expr'
    pure $ Unary (TBool, pos) Not expr'
inferIE (Unary pos Neg (Const _ (LFloat f))) =
    -- Negative float literals
    pure $ Const (TFloat, pos) (LFloat (-f))
inferIE (Unary pos Neg expr) = do
    expr' <- inferIE expr
    doUnify (getExprState expr) TInt $ getTy expr'
    pure $ Unary (TInt, pos) Neg expr'
inferIE (Unary pos FNeg expr) = do
    expr' <- inferIE expr
    doUnify (getExprState expr) TFloat $ getTy expr'
    pure $ Unary (TFloat, pos) FNeg expr'
inferIE (Binary pos (RelationOp op) expr1 expr2) = do
    expr1' <- inferIE expr1
    expr2' <- inferIE expr2
    doUnify (getExprState expr2) (getTy expr1') (getTy expr2')
    pure $ Binary (TBool, pos) (RelationOp op) expr1' expr2'
inferIE (Binary pos (IntOp op) expr1 expr2) = do
    expr1' <- inferIE expr1
    expr2' <- inferIE expr2
    doUnify (getExprState expr1) TInt (getTy expr1')
    doUnify (getExprState expr2) TInt (getTy expr2')
    pure $ Binary (TInt, pos) (IntOp op) expr1' expr2'
inferIE (Binary pos (FloatOp op) expr1 expr2) = do
    expr1' <- inferIE expr1
    expr2' <- inferIE expr2
    doUnify (getExprState expr1) TFloat (getTy expr1')
    doUnify (getExprState expr2) TFloat (getTy expr2')
    pure $ Binary (TFloat, pos) (FloatOp op) expr1' expr2'
inferIE (If pos (CIdentity cond) thenExpr elseExpr) = do
    cond' <- inferIE cond
    doUnify (getExprState cond) TBool (getTy cond')
    thenExpr' <- inferIE thenExpr
    elseExpr' <- inferIE elseExpr
    doUnify (getExprState elseExpr) (getTy thenExpr') (getTy elseExpr')
    pure $ If (getTy thenExpr', pos) (CIdentity cond') thenExpr' elseExpr'
inferIE (Let pos PUnit expr body) = do
    expr' <- inferIE expr
    body' <- inferIE body
    doUnify (getExprState expr) TUnit (getTy expr')
    pure $ Let (getTy body', pos) PUnit expr' body'
inferIE (Let pos (PVar v) expr body) = do
    expr' <- inferIE expr
    vId <- getTyOfIdent v
    doUnify (getExprState expr) (TVar vId) (getTy expr')
    body' <- inferIE body
    pure $ Let (getTy body', pos) (PVar v) expr' body'
inferIE (Let pos (PRec func args) expr body) = do
    expr' <- inferIE expr
    funcId <- getTyOfIdent func
    argIds <- mapM getTyOfIdent args
    doUnify (getExprState expr) (TVar funcId) (TFun (map TVar argIds) (getTy expr'))
    body' <- inferIE body
    pure $ Let (getTy body', pos) (PRec func args) expr' body'
inferIE (Let pos (PTuple values) expr body) = do
    expr' <- inferIE expr
    valueIds <- mapM getTyOfIdent values
    doUnify (getExprState expr) (TTuple $ map TVar valueIds) (getTy expr')
    body' <- inferIE body
    pure $ Let (getTy body', pos) (PTuple values) expr' body'
inferIE (Var pos v) = do
    vId <- getTyOfIdent v
    pure $ Var (TVar vId, pos) v
inferIE (App pos func args) = do
    func' <- inferIE func
    args' <- mapM inferIE args
    retId <- genNewId
    doUnify (getExprState func) (TFun (map getTy args') (TVar retId)) (getTy func')
    pure $ App (TVar retId, pos) func' args'
inferIE (Tuple pos values) = do
    values' <- mapM inferIE values
    pure $ Tuple (TTuple $ map getTy values', pos) values'
inferIE (ArrayCreate pos size initVal) = do
    size' <- inferIE size
    doUnify (getExprState size) TInt (getTy size')
    initVal' <- inferIE initVal
    pure $ ArrayCreate (TArray $ getTy initVal', pos) size' initVal'
inferIE (Get pos array idx) = do
    array' <- inferIE array
    idx' <- inferIE idx
    retId <- genNewId
    doUnify (getExprState array) (TArray $ TVar retId) (getTy array')
    doUnify (getExprState idx) TInt (getTy idx')
    pure $ Get (TVar retId, pos) array' idx'
inferIE (Put pos array idx value) = do
    array' <- inferIE array
    idx' <- inferIE idx
    value' <- inferIE value
    doUnify (getExprState array) (TArray $ getTy value') (getTy array')
    doUnify (getExprState idx) TInt (getTy idx')
    pure $ Put (TUnit, pos) array' idx' value'
