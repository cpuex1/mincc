{-# LANGUAGE OverloadedStrings #-}

module MiddleEnd.Optim (OptimKind (..), runOptim) where

import Display (Display (display))
import MiddleEnd.Optim.BoolOperation (boolOperation)
import MiddleEnd.Optim.CSE (runCSE)
import MiddleEnd.Optim.Common (OptimStateT)
import MiddleEnd.Optim.CompMerging (runMergeComp)
import MiddleEnd.Optim.ConstFold (constFold, constFoldFloat)
import MiddleEnd.Optim.IfMerging (mergeIf)
import MiddleEnd.Optim.Inlining (runInlining)
import MiddleEnd.Optim.LoopArgsElim (removeLoopArgs)
import MiddleEnd.Optim.LoopDetection (runReplaceWithLoops)
import MiddleEnd.Optim.ReadOnly (removeReadOnlyArrays)
import MiddleEnd.Optim.StripCondition (stripConditionM)
import MiddleEnd.Optim.SwapIf (swapIfM)
import MiddleEnd.Optim.UnusedElim (unusedElim)
import MiddleEnd.Optim.VarMerging (mergeVars)
import Syntax (KExpr)

-- | List of optimizations.
data OptimKind
    = BoolOperation
    | CompMerging
    | IfMerging
    | StripCondition
    | CSE
    | VarMerging
    | ConstFold
    | ConstFoldFloat
    | ReadOnly
    | UnusedElim
    | SwapIf
    | Inlining
    | LoopDetection
    | LoopArgsElim
    deriving (Show, Ord, Eq)

-- | Run the optimization.
runOptim :: (Monad m) => OptimKind -> KExpr -> OptimStateT m KExpr
runOptim BoolOperation = boolOperation
runOptim CompMerging = runMergeComp
runOptim IfMerging = mergeIf
runOptim StripCondition = stripConditionM
runOptim CSE = runCSE
runOptim VarMerging = mergeVars
runOptim ConstFold = constFold
runOptim ConstFoldFloat = constFoldFloat
runOptim Inlining = runInlining
runOptim ReadOnly = removeReadOnlyArrays
runOptim SwapIf = swapIfM
runOptim UnusedElim = unusedElim
runOptim LoopDetection = runReplaceWithLoops
runOptim LoopArgsElim = removeLoopArgs

instance Display OptimKind where
    display BoolOperation = "If expressions to boolean operations"
    display CompMerging = "Comparison merging"
    display IfMerging = "If expressions merging"
    display StripCondition = "Condition stripping"
    display LoopDetection = "Loop detection"
    display CSE = "Common subexpression elimination"
    display VarMerging = "Variable merging"
    display ConstFold = "Constant folding"
    display ConstFoldFloat = "Constant folding for floating-point numbers"
    display Inlining = "Function inlining"
    display ReadOnly = "Read-only arrays elimination"
    display SwapIf = "If expressions swapping"
    display UnusedElim = "Unused variables elimination"
    display LoopArgsElim = "Loop arguments elimination"
