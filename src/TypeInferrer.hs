{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeInferrer (
    genNewId,
    defaultEnv,
    inferType,
    TypeEnv (TypeEnv, assigned, table, variables),
    registerIdent,
    registerAll,
) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.State
import Data.Foldable (find, for_)
import Display
import Error (CompilerError (TypeError))
import Syntax
import Text.Megaparsec.Pos
import Typing

newtype ITypedExpr = ITGuard {iTExp :: Expr (ITy, SourcePos) Ident ITypedExpr}
    deriving (Show, Eq)

getTy :: ITypedExpr -> ITy
getTy = fst . getExprState . iTExp

data TypeEnv = TypeEnv
    { assigned :: Int
    , table :: [Maybe ITy]
    , variables :: [(Ident, TypeId)]
    }
    deriving (Show, Eq)

type TypingM = ExceptT CompilerError (State TypeEnv)

defaultEnv :: TypeEnv
defaultEnv = TypeEnv 0 [] []

genNewId :: TypingM TypeId
genNewId =
    get >>= \env ->
        modify (\e -> e{assigned = assigned e + 1, table = table e ++ [Nothing]})
            >> return (assigned env)

fromTypeId :: TypeId -> TypingM (Maybe ITy)
fromTypeId tId = get >>= \env -> return $ table env !! tId

registerIdent :: Ident -> TypingM ()
registerIdent ident = do
    env <- get
    case find ((== ident) . fst) $ variables env of
        Just _ -> pure ()
        Nothing -> do
            tId <- genNewId
            modify (\e -> e{variables = (ident, tId) : variables e})

registerAll :: ResolvedExpr -> TypingM ()
registerAll (RGuard expr) =
    void $ visitExprM pure (\ident -> registerIdent ident >> pure ident) registerAll expr

getTyOfIdent :: Ident -> TypingM TypeId
getTyOfIdent ident = do
    env <- get
    case find ((== ident) . fst) $ variables env of
        Just (_, tId) ->
            pure tId
        Nothing -> throwError $ TypeError Nothing "Detected an identifier not tracked"

updateEnv :: TypeId -> ITy -> TypingM ()
updateEnv typeId ty = do
    env <- get
    let env' =
            map
                ( \case
                    Just ty' -> Just $ updateTy typeId ty ty'
                    Nothing -> Nothing
                )
                $ replaceWithIdx typeId ty (table env)
     in modify (\e -> e{table = env'})
    pure ()
  where
    replaceWithIdx :: Int -> ITy -> [Maybe ITy] -> [Maybe ITy]
    replaceWithIdx _ _ [] = []
    replaceWithIdx 0 replaced (_ : xs) = Just replaced : xs
    replaceWithIdx idx replaced (x : xs) = x : replaceWithIdx (idx - 1) replaced xs

    updateTy :: TypeId -> ITy -> ITy -> ITy
    updateTy tId replaced (TVar tId')
        | tId == tId' = replaced
        | otherwise = TVar tId'
    updateTy tId replaced (TFun args ret) = TFun (map (updateTy tId replaced) args) (updateTy tId replaced ret)
    updateTy tId replaced (TTuple values) = TTuple $ map (updateTy tId replaced) values
    updateTy tId replaced (TArray value) = TArray $ updateTy tId replaced value
    updateTy _ _ ty' = ty'

applyEnv :: ITy -> TypingM Ty
applyEnv TUnit = pure TUnit
applyEnv TBool = pure TBool
applyEnv TInt = pure TInt
applyEnv TFloat = pure TFloat
applyEnv (TFun args ret) = TFun <$> mapM applyEnv args <*> applyEnv ret
applyEnv (TTuple values) = TTuple <$> mapM applyEnv values
applyEnv (TArray value) = TArray <$> applyEnv value
applyEnv (TVar tId) =
    fromTypeId tId >>= \case
        Just t -> applyEnv t
        Nothing -> pure TInt

applyEnvE :: ITypedExpr -> TypingM TypedExpr
applyEnvE (ITGuard expr) = do
    expr' <- visitExprM fState pure applyEnvE expr
    return $ TGuard expr'
  where
    fState :: (ITy, SourcePos) -> TypingM TypedState
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
        then throwError $ TypeError Nothing "Recursive type"
        else do
            ty <- fromTypeId tId'
            for_ ty $ \ty' -> occurCheck tId ty'

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
    if rTy == TVar tId
        then
            pure ()
        else do
            lTy <- fromTypeId tId
            case lTy of
                Just lTy' -> unify lTy' rTy
                Nothing -> do
                    occurCheck tId rTy
                    updateEnv tId rTy
unify lTy (TVar tId) = unify (TVar tId) lTy
unify _ _ = throwError $ TypeError Nothing "Type mismatch"

doUnify :: SourcePos -> ITy -> ITy -> TypingM ()
doUnify pos ty1 ty2 = do
    catchError (unify ty1 ty2) $
        const
            ( do
                expected <- applyEnv ty1
                actual <- applyEnv ty2
                throwError $ TypeError (Just pos) $ "Expected type " <> display expected <> " but got " <> display actual
            )

inferType :: ResolvedExpr -> Either CompilerError TypedExpr
inferType expr = evalState (runExceptT $ inferE expr) defaultEnv

inferE :: ResolvedExpr -> TypingM TypedExpr
inferE expr = do
    registerAll expr
    expr' <- inferIE expr
    if getTy expr' == TUnit
        then
            applyEnvE expr'
        else
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
inferIE (RGuard (If pos cond thenExpr elseExpr)) = do
    cond' <- inferIE cond
    doUnify (getExprState $ rExp cond) TBool (getTy cond')
    thenExpr' <- inferIE (RGuard thenExpr)
    elseExpr' <- inferIE (RGuard elseExpr)
    doUnify (getExprState elseExpr) (getTy thenExpr') (getTy elseExpr')
    pure $ ITGuard $ If (getTy thenExpr', pos) cond' (iTExp thenExpr') (iTExp elseExpr')
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
