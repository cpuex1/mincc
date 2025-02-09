{-# LANGUAGE GADTs #-}

module MiddleEnd.Analysis.Constant (registerConstants) where

import MiddleEnd.Analysis.Identifier (IdentEnvT, IdentProp (constant), updateProp)
import Syntax (Expr (Const, If, Let, Loop), KExpr, Pattern (PRec, PVar))

-- | Registers variables that hold a constant value.
registerConstants :: (Monad m) => KExpr -> IdentEnvT m ()
registerConstants (Let _ (PVar v) (Const _ lit) body) = do
    updateProp v (\p -> p{constant = Just lit})
    registerConstants body
registerConstants (Let _ (PRec _ _) expr body) = do
    registerConstants expr
    registerConstants body
registerConstants (Let _ _ expr body) = do
    registerConstants expr
    registerConstants body
registerConstants (If _ _ t f) = do
    registerConstants t
    registerConstants f
registerConstants (Loop _ _ _ body) = do
    registerConstants body
registerConstants _ = pure ()
