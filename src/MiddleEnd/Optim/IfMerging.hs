{-# LANGUAGE GADTs #-}

module MiddleEnd.Optim.IfMerging (mergeIf) where

import FrontEnd.Flatten (flattenExpr)
import MiddleEnd.Optim.Common (OptimStateT, purge, repeatOptim)
import Syntax (
    Expr (If, Let, Loop),
    KExpr,
    Pattern (PUnit, PVar),
    subst,
 )

mergeIf :: (Monad m) => KExpr -> OptimStateT m KExpr
mergeIf = repeatOptim (pure . flattenExpr . mergeIf')
  where
    mergeIf' :: KExpr -> KExpr
    mergeIf' (If state cond thenE@(Let _ (PVar v1) expr1 body1) elseE@(Let _ (PVar v2) expr2 body2)) =
        -- The condition cannot be changed by the side effect of `expr1`.
        if purge expr1 == purge expr2
            then
                let newBody2 = subst v2 v1 body2
                 in Let state (PVar v1) expr1 (If state cond body1 newBody2)
            else
                If state cond (mergeIf' thenE) (mergeIf' elseE)
    mergeIf' (If state cond thenE@(Let _ PUnit expr1 body1) elseE@(Let _ PUnit expr2 body2)) =
        -- The condition cannot be changed by the side effect of `expr1`.
        if purge expr1 == purge expr2
            then
                Let state PUnit expr1 (If state cond body1 body2)
            else
                If state cond (mergeIf' thenE) (mergeIf' elseE)
    mergeIf' (If state cond thenE elseE) =
        if purge thenE == purge elseE
            then
                thenE
            else
                If state cond (mergeIf' thenE) (mergeIf' elseE)
    mergeIf' (Let state patten expr body) = Let state patten (mergeIf' expr) (mergeIf' body)
    mergeIf' (Loop state args values body) = Loop state args values (mergeIf' body)
    mergeIf' inst = inst
