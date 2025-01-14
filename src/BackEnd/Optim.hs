{-# LANGUAGE OverloadedStrings #-}

module BackEnd.Optim (
    BackEndOptimKind (..),
    runBackEndOptim,
    Display,
) where

import BackEnd.Optim.Common (BackEndOptimStateT)
import BackEnd.Optim.MulElim (elimMul)
import Display (Display (display))
import IR (IntermediateCodeBlock)

-- | List of optimizations.
data BackEndOptimKind
    = MulElim
    deriving (Show, Ord, Eq)

-- | Run the optimization.
runBackEndOptim :: (Monad m) => BackEndOptimKind -> IntermediateCodeBlock stateTy idTy -> BackEndOptimStateT m (IntermediateCodeBlock stateTy idTy)
runBackEndOptim MulElim = elimMul

instance Display BackEndOptimKind where
    display MulElim = "Multiply elimination"
