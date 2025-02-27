{-# LANGUAGE GADTs #-}

module FrontEnd.KNorm (
    kNormalize,
) where

import MiddleEnd.Analysis.Identifier (IdentEnvT, genNewVar)
import Syntax
import Typing (removeBoolTy)

-- | Insert a let expression to a tail of the K-normalized expression.
insertLet :: (Monad m) => KExpr -> (Ident -> IdentEnvT m KExpr) -> IdentEnvT m KExpr
insertLet (Var _ v) genBody = genBody v
insertLet expr genBody = do
    newVar <- genNewVar $ getType $ getExprState expr
    body <- genBody newVar
    pure $ Let (getExprState body) (PVar newVar) expr body

insertLetAll :: (Monad m) => [KExpr] -> ([Ident] -> IdentEnvT m KExpr) -> IdentEnvT m KExpr
insertLetAll = insertLetAll' []
  where
    insertLetAll' :: (Monad m) => [Ident] -> [KExpr] -> ([Ident] -> IdentEnvT m KExpr) -> IdentEnvT m KExpr
    insertLetAll' idents [] genBody = genBody idents
    insertLetAll' idents (e : exprs) genBody = do
        insertLet e (\ident -> insertLetAll' (idents ++ [ident]) exprs genBody)

{- | Converts an expression to a K-normalized expression.
This function also marks bool-typed variables as int-typed ones.
-}
kNormalize :: (Monad m) => TypedExpr -> IdentEnvT m KExpr
kNormalize (Const (TState ty loc) LUnit) = pure (Const (TState (removeBoolTy ty) loc) LUnit)
kNormalize (Const (TState ty loc) (LBool b)) = pure (Const (TState (removeBoolTy ty) loc) (LInt $ if b then 1 else 0))
kNormalize (Const (TState ty loc) (LInt i)) = pure (Const (TState (removeBoolTy ty) loc) (LInt i))
kNormalize (Const (TState ty loc) (LFloat f)) = pure (Const (TState (removeBoolTy ty) loc) (LFloat f))
kNormalize (Unary (TState ty loc) op expr) = do
    expr' <- kNormalize expr
    insertLet expr' (pure . Unary (TState (removeBoolTy ty) loc) op)
kNormalize (Binary (TState ty loc) op lhs rhs) = do
    lhs' <- kNormalize lhs
    rhs' <- kNormalize rhs
    insertLet
        lhs'
        ( \ident ->
            insertLet rhs' (pure . Binary (TState (removeBoolTy ty) loc) op ident)
        )
kNormalize (If (TState ty loc) (CIdentity cond) thenE elseE) = do
    cond' <- kNormalize cond
    insertLet
        cond'
        ( \ident -> do
            thenE' <- kNormalize thenE
            elseE' <- kNormalize elseE
            pure $ If (TState (removeBoolTy ty) loc) (CIdentity ident) thenE' elseE'
        )
kNormalize (Let (TState ty loc) pat value body) = do
    value' <- kNormalize value
    body' <- kNormalize body
    pure $ Let (TState (removeBoolTy ty) loc) pat value' body'
kNormalize (Var (TState ty loc) v) = do
    pure $ Var (TState (removeBoolTy ty) loc) v
kNormalize (App (TState ty loc) func args) = do
    func' <- kNormalize func
    args' <- mapM kNormalize args
    insertLet
        func'
        ( \fIdent ->
            insertLetAll args' (pure . App (TState (removeBoolTy ty) loc) fIdent)
        )
kNormalize (Tuple (TState ty loc) values) = do
    values' <- mapM kNormalize values
    insertLetAll values' (pure . Tuple (TState (removeBoolTy ty) loc))
kNormalize (ArrayCreate (TState ty loc) size value) = do
    size' <- kNormalize size
    value' <- kNormalize value
    insertLet
        size'
        ( \sizeI ->
            insertLet value' (pure . ArrayCreate (TState (removeBoolTy ty) loc) sizeI)
        )
kNormalize (Get (TState ty loc) array index) = do
    array' <- kNormalize array
    index' <- kNormalize index
    insertLet
        array'
        ( \arrayI ->
            insertLet index' (pure . Get (TState (removeBoolTy ty) loc) arrayI)
        )
kNormalize (Put (TState ty loc) array index value) = do
    array' <- kNormalize array
    index' <- kNormalize index
    value' <- kNormalize value
    insertLet
        array'
        ( \arrayI ->
            insertLet
                index'
                ( \indexI ->
                    insertLet value' (pure . Put (TState (removeBoolTy ty) loc) arrayI indexI)
                )
        )
