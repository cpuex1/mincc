{-# LANGUAGE OverloadedStrings #-}

module MiddleEnd.Optim (OptimKind (..), runOptim) where

import Display (Display (display))
import MiddleEnd.Optim.CSE (runCSE)
import MiddleEnd.Optim.Common (OptimStateT)
import MiddleEnd.Optim.CompMerging (runMergeComp)
import MiddleEnd.Optim.ConstFold (constFold, constFoldFloat)
import MiddleEnd.Optim.Inlining (runInlining)
import MiddleEnd.Optim.UnusedElim (unusedElim)
import MiddleEnd.Optim.VarMerging (mergeVars)
import Syntax (KExpr)

-- | List of optimizations.
data OptimKind
    = CompMerging
    | CSE
    | VarMerging
    | ConstFold
    | ConstFoldFloat
    | UnusedElim
    | Inlining
    deriving (Show, Ord, Eq)

-- | Run the optimization.
runOptim :: (Monad m) => OptimKind -> KExpr -> OptimStateT m KExpr
runOptim CompMerging = runMergeComp
runOptim CSE = runCSE
runOptim VarMerging = mergeVars
runOptim ConstFold = constFold
runOptim ConstFoldFloat = constFoldFloat
runOptim Inlining = runInlining
runOptim UnusedElim = unusedElim

instance Display OptimKind where
    display CompMerging = "Comparison merging"
    display CSE = "Common subexpression elimination"
    display VarMerging = "Variable merging"
    display ConstFold = "Constant folding"
    display ConstFoldFloat = "Constant folding for floating-point numbers"
    display Inlining = "Function inlining"
    display UnusedElim = "Unused variables elimination"
