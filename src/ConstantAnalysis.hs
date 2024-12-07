{-# LANGUAGE GADTs #-}

module ConstantAnalysis (registerConstants) where

import IdentAnalysis (IdentEnvT, IdentProp (constant), updateProp)
import Syntax (Expr (Const, If, Let), KExpr, Pattern (PVar))

-- | Registers variables that hold a constant value.
registerConstants :: (Monad m) => KExpr -> IdentEnvT m ()
registerConstants (Let _ (PVar v) (Const _ lit) body) = do
    updateProp v (\p -> p{constant = Just lit})
    registerConstants body
registerConstants (Let _ _ _ body) = registerConstants body
registerConstants (If _ _ t f) = do
    registerConstants t
    registerConstants f
registerConstants _ = pure ()
