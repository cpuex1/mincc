{-# LANGUAGE GADTs #-}

module Optim.Inlining () where

import Control.Monad.Reader (Reader, asks, runReader)
import Data.Map (Map, lookup)
import Flatten (flattenExpr)
import Syntax (Expr (App, If, Let), Ident, KExpr, subst)
import Prelude hiding (lookup)

data InlineTarget = InlineTarget
    { targetArgs :: [Ident]
    , targetExpr :: KExpr
    }
    deriving (Show, Eq)

newtype InlineContext = InlineContext
    { targets :: Map Ident InlineTarget
    }
    deriving (Show, Eq)

type InliningM = Reader InlineContext

inlining :: KExpr -> InliningM KExpr
inlining (App state func args) = do
    targets' <- asks targets
    case lookup func targets' of
        Just (InlineTarget targetArgs' targetExpr') ->
            pure $
                foldl (\e (from, to) -> subst from to from to e) targetExpr' $
                    zip args targetArgs'
        Nothing ->
            pure $ App state func args
inlining (If state cond thenExpr elseExpr) = do
    thenExpr' <- inlining thenExpr
    elseExpr' <- inlining elseExpr
    pure $ If state cond thenExpr' elseExpr'
inlining (Let state pat expr body) = do
    expr' <- inlining expr
    body' <- inlining body
    pure $ flattenExpr $ Let state pat expr' body'
inlining expr = pure expr
