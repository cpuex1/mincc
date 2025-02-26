{-# LANGUAGE OverloadedStrings #-}

module BackEnd.Optim (
    BackEndOptimKind (..),
    runBackEndOptim,
    Display,
) where

import BackEnd.Optim.Common (BackEndOptimStateT)
import BackEnd.Optim.EmptyBlockMerging (mergeEmptyBlockM)
import BackEnd.Optim.Merging (mergeBlocks)
import BackEnd.Optim.MulElim (elimMul)
import BackEnd.Optim.RegMerging (regMerging)
import BackEnd.Optim.Unreachable (removeUnreachable)
import BackEnd.Optim.UnusedReg (removeUnusedReg)
import CodeBlock (VirtualBlockGraph)
import Display (Display (display))

-- | List of optimizations.
data BackEndOptimKind
    = Unreachable
    | Merging
    | EmptyBlockMerging
    | RegMerging
    | UnusedReg
    | MulElim
    deriving (Show, Ord, Eq)

-- | Run the optimization.
runBackEndOptim :: (Monad m) => BackEndOptimKind -> VirtualBlockGraph -> BackEndOptimStateT m VirtualBlockGraph
runBackEndOptim Unreachable = removeUnreachable
runBackEndOptim Merging = mergeBlocks
runBackEndOptim EmptyBlockMerging = mergeEmptyBlockM
runBackEndOptim RegMerging = regMerging
runBackEndOptim UnusedReg = removeUnusedReg
runBackEndOptim MulElim = elimMul

instance Display BackEndOptimKind where
    display Unreachable = "Remove unreachable blocks"
    display Merging = "Code blocks merging"
    display EmptyBlockMerging = "Empty block merging"
    display RegMerging = "Register merging"
    display UnusedReg = "Remove unused registers"
    display MulElim = "Multiply elimination"
