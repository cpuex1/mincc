{-# LANGUAGE GADTs #-}

module FrontEnd.KNorm (
    kNormalize,
) where

import MiddleEnd.Analysis.Identifier (IdentEnvT, genNewVar)
import Syntax

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

kNormalize :: (Monad m) => TypedExpr -> IdentEnvT m KExpr
kNormalize (Const s lit) = pure (Const s lit)
kNormalize (Unary s op expr) = do
    expr' <- kNormalize expr
    insertLet expr' (pure . Unary s op)
kNormalize (Binary s op lhs rhs) = do
    lhs' <- kNormalize lhs
    rhs' <- kNormalize rhs
    insertLet
        lhs'
        ( \ident ->
            insertLet rhs' (pure . Binary s op ident)
        )
kNormalize (If s (CIdentity cond) thenE elseE) = do
    cond' <- kNormalize cond
    insertLet
        cond'
        ( \ident -> do
            thenE' <- kNormalize thenE
            elseE' <- kNormalize elseE
            pure $ If s (CIdentity ident) thenE' elseE'
        )
kNormalize (Let s pat value body) = do
    value' <- kNormalize value
    body' <- kNormalize body
    pure $ Let s pat value' body'
kNormalize (Var s v) = do
    pure $ Var s v
kNormalize (App s func args) = do
    func' <- kNormalize func
    args' <- mapM kNormalize args
    insertLet
        func'
        ( \fIdent ->
            insertLetAll args' (pure . App s fIdent)
        )
kNormalize (Tuple s values) = do
    values' <- mapM kNormalize values
    insertLetAll values' (pure . Tuple s)
kNormalize (ArrayCreate s size value) = do
    size' <- kNormalize size
    value' <- kNormalize value
    insertLet
        size'
        ( \sizeI ->
            insertLet value' (pure . ArrayCreate s sizeI)
        )
kNormalize (Get s array index) = do
    array' <- kNormalize array
    index' <- kNormalize index
    insertLet
        array'
        ( \arrayI ->
            insertLet index' (pure . Get s arrayI)
        )
kNormalize (Put s array index value) = do
    array' <- kNormalize array
    index' <- kNormalize index
    value' <- kNormalize value
    insertLet
        array'
        ( \arrayI ->
            insertLet
                index'
                ( \indexI ->
                    insertLet value' (pure . Put s arrayI indexI)
                )
        )
