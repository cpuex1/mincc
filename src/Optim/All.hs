{-# LANGUAGE OverloadedStrings #-}

module Optim.All (OptimKind (..), runOptim) where

import Display (Display (display))
import Optim.Base (OptimStateT)
import Optim.CompMerging (runMergeComp)
import Optim.ConstFold (constFold)
import Optim.Inlining (runInlining)
import Optim.UnusedElim (unusedElim)
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
