{-# LANGUAGE GADTs #-}

module MiddleEnd.Optim.BoolOperation (
    boolOperation,
) where

import Control.Monad.Trans (MonadTrans (lift))
import FrontEnd.Flatten (flattenExpr)
import MiddleEnd.Analysis.Identifier (genNewVar)
import MiddleEnd.Optim.Common (OptimStateT)
import Syntax (
    BinaryOp (IntOp, RelationOp),
    Cond (CComp, CIdentity, CNeg),
    Expr (..),
    IntBinOp (Xor),
    KExpr,
    Pattern (PVar),
    UnaryOp (Not),
 )
import Typing (TypeBase (TInt))

boolOperation :: (Monad m) => KExpr -> OptimStateT m KExpr
boolOperation inst@(If state (CIdentity cond) (Unary _ Not v1) (Var _ v2)) =
    if v1 == v2
        then
            pure $ Binary state (IntOp Xor) cond v1
        else
            pure inst
boolOperation inst@(If state (CNeg cond) (Var _ v1) (Unary _ Not v2)) =
    if v1 == v2
        then
            pure $ Binary state (IntOp Xor) cond v1
        else
            pure inst
boolOperation inst@(If state (CComp op lhs rhs) (Unary _ Not v1) (Var _ v2)) =
    if v1 == v2
        then do
            cond <- lift $ genNewVar TInt
            pure $ Let state (PVar cond) (Binary state (RelationOp op) lhs rhs) (Binary state (IntOp Xor) cond v1)
        else
            pure inst
boolOperation (If state cond lhs rhs) = do
    lhs' <- boolOperation lhs
    rhs' <- boolOperation rhs
    pure $ If state cond lhs' rhs'
boolOperation (Let state pat expr body) = do
    expr' <- boolOperation expr
    body' <- boolOperation body
    pure $ flattenExpr $ Let state pat expr' body'
boolOperation (Loop state args values body) = do
    body' <- boolOperation body
    pure $ Loop state args values body'
boolOperation inst = pure inst
