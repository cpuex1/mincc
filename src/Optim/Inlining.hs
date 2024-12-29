{-# LANGUAGE GADTs #-}

module Optim.Inlining (runInlining) where

import Control.Monad (when)
import Control.Monad.State (
    MonadTrans (lift),
    StateT,
    evalStateT,
    gets,
    modify,
 )
import Data.Map (Map, empty, insert, lookup)
import Flatten (flattenExpr)
import Optim.Base (OptimContext (inliningSizeThreshold, recInliningLimit, recInliningSizeThreshold), OptimStateT, exprSize, isUsed, withFreshVars)
import Syntax (Expr (App, If, Let), Ident, KExpr, Pattern (PRec), subst)
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

type InliningM m = StateT InlineContext m

registerTarget :: (Monad m) => Ident -> [Ident] -> KExpr -> InliningM m ()
registerTarget func args expr = do
    modify $ \ctx ->
        ctx{targets = insert func (InlineTarget args expr) (targets ctx)}

tryRegister :: (Monad m) => Ident -> [Ident] -> KExpr -> InliningM (OptimStateT m) ()
tryRegister func args expr =
    if isUsed func expr
        then do
            -- Recursive function
            recInliningLimit' <- lift $ gets recInliningLimit
            recInliningSizeThreshold' <- lift $ gets recInliningSizeThreshold
            when (recInliningLimit' > 0 && size <= recInliningSizeThreshold') $
                registerTarget func args expr
        else do
            -- Non-recursive function
            inliningSizeThreshold' <- lift $ gets inliningSizeThreshold
            when (size <= inliningSizeThreshold') $
                registerTarget func args expr
  where
    size = exprSize expr

inlining :: (Monad m) => KExpr -> InliningM (OptimStateT m) KExpr
inlining (App state func args) = do
    targets' <- gets targets
    case lookup func targets' of
        Just (InlineTarget targetArgs' targetExpr') ->
            -- Perform inlining.
            lift $
                withFreshVars $
                    foldl (\e (from, to) -> subst from to from to e) targetExpr' $
                        zip targetArgs' args
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

-- | Finds inlining targets.
findTargets :: (Monad m) => KExpr -> InliningM (OptimStateT m) ()
findTargets (Let _ (PRec func args) expr body) = do
    tryRegister func args expr
    findTargets expr
    findTargets body
findTargets (Let _ _ expr body) = do
    findTargets expr
    findTargets body
findTargets (If _ _ then' else') = do
    findTargets then'
    findTargets else'
findTargets _ = pure ()

runInlining :: (Monad m) => KExpr -> OptimStateT m KExpr
runInlining expr = do
    result <-
        evalStateT
            ( do
                findTargets expr
                inlining expr
            )
            $ InlineContext empty
    modify
        ( \ctx ->
            ctx{recInliningLimit = recInliningLimit ctx - 1}
        )
    pure result
