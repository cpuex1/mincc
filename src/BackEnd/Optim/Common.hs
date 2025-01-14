module BackEnd.Optim.Common (
    BackEndOptimContext (..),
    BackEndOptimStateT,
) where

import BackEnd.BackendEnv (BackendStateT)
import Control.Monad.State (StateT)

data BackEndOptimContext = BackEndOptimContext
    deriving (Show, Eq)

type BackEndOptimStateT m = StateT BackEndOptimContext (BackendStateT m)
