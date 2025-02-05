{-# LANGUAGE GADTs #-}

module MiddleEnd.Optim.VarMerging (mergeVars) where

import Control.Monad.Trans (MonadTrans (lift))
import MiddleEnd.Analysis.Identifier (removeProp)
import MiddleEnd.Optim.Common (OptimStateT)
import Syntax (Expr (..), KExpr, Pattern (PVar), subst)

mergeVars :: (Monad m) => KExpr -> OptimStateT m KExpr
mergeVars (Let _ (PVar v1) (Var _ v2) body) = do
    body' <- mergeVars body
    lift $ removeProp v1
    pure $ subst v1 v2 body'
mergeVars (Let state pat expr body) = do
    expr' <- mergeVars expr
    body' <- mergeVars body
    pure $ Let state pat expr' body'
mergeVars (If state cond thenExpr elseExpr) = do
    thenExpr' <- mergeVars thenExpr
    elseExpr' <- mergeVars elseExpr
    pure $ If state cond thenExpr' elseExpr'
mergeVars (Loop state args value body) = do
    body' <- mergeVars body
    pure $ Loop state args value body'
mergeVars expr = pure expr
