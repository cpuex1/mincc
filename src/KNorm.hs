{-# LANGUAGE GADTs #-}
module KNorm (
    OptimState,
    OptimEnv (OptimEnv, generated),
    defaultOptimEnv,
    genVar,
    kNormalize,
) where

import Control.Monad.State
import Syntax

newtype OptimEnv = OptimEnv
    {generated :: Int}
    deriving (Show, Eq)

defaultOptimEnv :: OptimEnv
defaultOptimEnv = OptimEnv 0

type OptimState = State OptimEnv

genVar :: OptimState Ident
genVar = do
    env <- get
    modify (\e -> e{generated = generated e + 1})
    pure $ CompilerGenerated $ generated env

-- | Insert a let expression to a tail of the K-normalized expression.
insertLet :: KExpr -> (Ident -> OptimState KExpr) -> OptimState KExpr
insertLet (Var _ v) genBody = genBody v
insertLet expr genBody = do
    newVar <- genVar
    body <- genBody newVar
    pure $ Let (getExprState body) (PVar newVar) expr body

insertLetAll :: [KExpr] -> ([Ident] -> OptimState KExpr) -> OptimState KExpr
insertLetAll = insertLetAll' []
  where
    insertLetAll' :: [Ident] -> [KExpr] -> ([Ident] -> OptimState KExpr) -> OptimState KExpr
    insertLetAll' idents [] genBody = genBody idents
    insertLetAll' idents (e : exprs) genBody = do
        insertLet e (\ident -> insertLetAll' (idents ++ [ident]) exprs genBody)

kNormalize :: TypedExpr -> OptimState KExpr
kNormalize (TGuard (Const s lit)) = pure (Const s lit)
kNormalize (TGuard (Unary s op expr)) = do
    expr' <- kNormalize expr
    insertLet expr' (pure . Unary s op)
kNormalize (TGuard (Binary s op lhs rhs)) = do
    lhs' <- kNormalize lhs
    rhs' <- kNormalize rhs
    insertLet
        lhs'
        ( \ident ->
            insertLet rhs' (pure . Binary s op ident)
        )
kNormalize (TGuard (If s cond thenE elseE)) = do
    cond' <- kNormalize cond
    insertLet
        cond'
        ( \ident -> do
            thenE' <- kNormalize (TGuard thenE)
            elseE' <- kNormalize (TGuard elseE)
            pure $ If s ident thenE' elseE'
        )
kNormalize (TGuard (Let s pat value body)) = do
    value' <- kNormalize (TGuard value)
    body' <- kNormalize (TGuard body)
    pure $ Let s pat value' body'
kNormalize (TGuard (Var s v)) = do
    pure $ Var s v
kNormalize (TGuard (App s func args)) = do
    func' <- kNormalize func
    args' <- mapM kNormalize args
    insertLet
        func'
        ( \fIdent ->
            insertLetAll args' (pure . App s fIdent)
        )
kNormalize (TGuard (Tuple s values)) = do
    values' <- mapM kNormalize values
    insertLetAll values' (pure . Tuple s)
kNormalize (TGuard (ArrayCreate s size value)) = do
    size' <- kNormalize size
    value' <- kNormalize value
    insertLet
        size'
        ( \sizeI ->
            insertLet value' (pure . ArrayCreate s sizeI)
        )
kNormalize (TGuard (Get s array index)) = do
    array' <- kNormalize array
    index' <- kNormalize index
    insertLet
        array'
        ( \arrayI ->
            insertLet index' (pure . Get s arrayI)
        )
kNormalize (TGuard (Put s array index value)) = do
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
