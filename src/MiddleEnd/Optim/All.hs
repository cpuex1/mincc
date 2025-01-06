{-# LANGUAGE OverloadedStrings #-}

module MiddleEnd.Optim.All (OptimKind (..), runOptim) where

import Display (Display (display))
import MiddleEnd.Optim.Base (OptimStateT)
import MiddleEnd.Optim.CompMerging (runMergeComp)
import MiddleEnd.Optim.ConstFold (constFold)
import MiddleEnd.Optim.Inlining (runInlining)
import MiddleEnd.Optim.UnusedElim (unusedElim)
import Syntax (KExpr)

-- | List of optimizations.
data OptimKind
    = CompMerging
    | ConstFold
    | UnusedElim
    | Inlining
    deriving (Show, Ord, Eq)

-- | Run the optimization.
runOptim :: (Monad m) => OptimKind -> KExpr -> OptimStateT m KExpr
runOptim CompMerging = runMergeComp
runOptim ConstFold = constFold
runOptim Inlining = runInlining
runOptim UnusedElim = unusedElim

instance Display OptimKind where
    display CompMerging = "Comparison merging"
    display ConstFold = "Constant folding"
    display Inlining = "Function inlining"
    display UnusedElim = "Unused variables elimination"
