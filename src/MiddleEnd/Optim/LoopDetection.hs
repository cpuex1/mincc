{-# LANGUAGE GADTs #-}

module MiddleEnd.Optim.LoopDetection (runReplaceWithLoops) where

import Control.Monad (when)
import Control.Monad.State (MonadTrans (lift), StateT, evalStateT, gets, modify)
import Data.Map (Map, insert, lookup)
import MiddleEnd.Analysis.Identifier (genNewVar, getTyOf)
import MiddleEnd.Optim.Common (OptimStateT, occur, withFreshVars)
import Syntax (Expr (..), Ident, KExpr, Pattern (PRec, PTuple, PUnit, PVar), subst)
import Prelude hiding (lookup)

newtype LoopContext = LoopContext
    { loop :: Map Ident ([Ident], KExpr)
    }
    deriving (Show, Eq)

type LoopStateT m = StateT LoopContext (OptimStateT m)

detectLoop :: Ident -> KExpr -> (Bool, KExpr)
detectLoop func (Let state PUnit expr body) =
    (bodyLoop, Let state PUnit expr body')
  where
    (bodyLoop, body') = detectLoop func body
detectLoop func (Let state (PVar v) expr body) =
    (not (occur func expr) && bodyLoop, Let state (PVar v) expr body')
  where
    (bodyLoop, body') = detectLoop func body
detectLoop func (Let state (PTuple values) expr body) =
    (not (occur func expr) && bodyLoop, Let state (PTuple values) expr body')
  where
    (bodyLoop, body') = detectLoop func body
detectLoop _ expr@(Let _ (PRec _ _) _ _) =
    -- If the function contains a definition, it should not be replicated.
    (False, expr)
detectLoop func (If state cond then' else') =
    (thenLoop || elseLoop, If state cond then'' else'')
  where
    (thenLoop, then'') = detectLoop func then'
    (elseLoop, else'') = detectLoop func else'
detectLoop _ expr@Loop{} =
    -- Nested loop is currently not supported.
    (False, expr)
detectLoop func (App state func' values) =
    if func == func'
        then
            (True, Continue state values)
        else
            (False, App state func' values)
detectLoop _ expr = (False, expr)

runReplaceWithLoops :: (Monad m) => KExpr -> OptimStateT m KExpr
runReplaceWithLoops expr =
    evalStateT (replaceWithLoops expr) $ LoopContext mempty

replaceWithLoops :: (Monad m) => KExpr -> LoopStateT m KExpr
replaceWithLoops (Let state PUnit expr body) = do
    expr' <- replaceWithLoops expr
    body' <- replaceWithLoops body
    pure $ Let state PUnit expr' body'
replaceWithLoops (Let state (PVar v) expr body) = do
    expr' <- replaceWithLoops expr
    body' <- replaceWithLoops body
    pure $ Let state (PVar v) expr' body'
replaceWithLoops (Let state (PTuple values) expr body) = do
    expr' <- replaceWithLoops expr
    body' <- replaceWithLoops body
    pure $ Let state (PTuple values) expr' body'
replaceWithLoops (Let state (PRec func args) expr body) = do
    when isLoop $ do
        modify $ \ctx ->
            ctx{loop = insert func (args, loopBody) (loop ctx)}
    body' <- replaceWithLoops body
    pure $ Let state (PRec func args) expr body'
  where
    (isLoop, loopBody) = detectLoop func expr
replaceWithLoops (If state cond then' else') = do
    then'' <- replaceWithLoops then'
    else'' <- replaceWithLoops else'
    pure $ If state cond then'' else''
replaceWithLoops (Loop state args values body) = do
    -- TODO: Support nested loops
    pure $ Loop state args values body
replaceWithLoops (App state func values) = do
    result <- gets (lookup func . loop)
    case result of
        Just (args', body) -> do
            newArgs <-
                mapM
                    ( \arg -> do
                        ty <- lift $ lift $ getTyOf arg
                        fresh <- lift $ lift $ genNewVar ty
                        pure (arg, fresh)
                    )
                    args'
            let subst' = foldl (\e (from, to) -> subst from to e) body newArgs
            body' <- lift $ withFreshVars subst'
            pure $ Loop state (map snd newArgs) values body'
        Nothing -> pure $ App state func values
replaceWithLoops expr = pure expr
