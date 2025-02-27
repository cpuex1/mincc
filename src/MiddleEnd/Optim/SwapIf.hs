{-# LANGUAGE GADTs #-}

module MiddleEnd.Optim.SwapIf (swapIfM) where

import MiddleEnd.Optim.Common (OptimStateT)
import Syntax (Cond (CComp, CIdentity, CNeg), Expr (..), KExpr, Literal (LUnit), negateRelation)

-- Due to the implementation of the back-end,
-- minimizing "then" expressions rather than "else" expressions can improve the performance.
-- This optimization tries to minimize it by swapping them.

swapIfM :: (Monad m) => KExpr -> OptimStateT m KExpr
swapIfM = pure . swapIf

swapIf :: KExpr -> KExpr
swapIf (If state (CComp op lhs rhs) thenE (Const state' LUnit)) =
    If state (CComp (negateRelation op) lhs rhs) (Const state' LUnit) (swapIf thenE)
swapIf (If state (CIdentity cond) thenE (Const state' LUnit)) =
    If state (CNeg cond) (Const state' LUnit) (swapIf thenE)
swapIf (If state (CNeg cond) thenE (Const state' LUnit)) =
    If state (CIdentity cond) (Const state' LUnit) (swapIf thenE)
swapIf (If state cond thenE elseE) =
    If state cond (swapIf thenE) (swapIf elseE)
swapIf (Let state patten expr body) = Let state patten (swapIf expr) (swapIf body)
swapIf (Loop state args values body) = Loop state args values (swapIf body)
swapIf inst = inst
