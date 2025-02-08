{-# LANGUAGE OverloadedStrings #-}

module BackEnd.Optim (
    BackEndOptimKind (..),
    runBackEndOptim,
    Display,
) where

import BackEnd.Optim.Common (BackEndOptimStateT)
import BackEnd.Optim.MulElim (elimMul)
import Display (Display (display))
import IR (AbstCodeBlock)

-- | List of optimizations.
data BackEndOptimKind
    = MulElim
    deriving (Show, Ord, Eq)

-- | Run the optimization.
runBackEndOptim :: (Monad m) => BackEndOptimKind -> AbstCodeBlock -> BackEndOptimStateT m AbstCodeBlock
runBackEndOptim MulElim = elimMul

instance Display BackEndOptimKind where
    display MulElim = "Multiply elimination"
