{-# LANGUAGE OverloadedStrings #-}

module BackEnd.Optim (
    BackEndOptimKind (..),
    runBackEndOptim,
    Display,
) where

import BackEnd.Optim.Common (BackEndOptimStateT)
import BackEnd.Optim.MulElim (elimMul)
import BackEnd.Optim.Unreachable (removeUnreachable)
import CodeBlock (VirtualBlockGraph)
import Display (Display (display))

-- | List of optimizations.
data BackEndOptimKind
    = Unreachable
    | MulElim
    deriving (Show, Ord, Eq)

-- | Run the optimization.
runBackEndOptim :: (Monad m) => BackEndOptimKind -> VirtualBlockGraph -> BackEndOptimStateT m VirtualBlockGraph
runBackEndOptim Unreachable = removeUnreachable
runBackEndOptim MulElim = elimMul

instance Display BackEndOptimKind where
    display Unreachable = "Remove unreachable blocks"
    display MulElim = "Multiply elimination"
