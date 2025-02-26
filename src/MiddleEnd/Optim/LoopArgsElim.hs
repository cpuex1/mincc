{-# LANGUAGE GADTs #-}

module MiddleEnd.Optim.LoopArgsElim (removeLoopArgs) where

import Control.Monad (when)
import Control.Monad.State (State, execState, gets, modify)
import Data.Set (Set, delete, fromList, member, notMember)
import MiddleEnd.Optim.Common (OptimStateT)
import Syntax (Expr (Continue, If, Let, Loop), Ident, KExpr, subst)

data LoopArgsContext
    = LoopArgsContext
    { arguments :: [Ident]
    , reused :: Set Ident
    }

type LoopArgsState = State LoopArgsContext

-- | Collects the information of arguments usages.
visitLoopBody :: KExpr -> LoopArgsState ()
visitLoopBody (Continue _ args') = do
    args <- gets arguments
    mapM_
        ( \(a, a') ->
            when (a /= a') $ do
                modify $ \ctx -> ctx{reused = delete a $ reused ctx}
        )
        $ zip args args'
visitLoopBody (Loop{}) = pure ()
visitLoopBody (If _ _ then' else') = do
    visitLoopBody then'
    visitLoopBody else'
visitLoopBody (Let _ _ _ body) =
    visitLoopBody body
visitLoopBody _ = pure ()

-- | Eliminates the unused arguments.
modifyContinue :: Set Ident -> KExpr -> KExpr
modifyContinue targets (Continue state args) =
    Continue state (filter (`notMember` targets) args)
modifyContinue targets (If state cond then' else') =
    If state cond (modifyContinue targets then') (modifyContinue targets else')
modifyContinue targets (Let state pat expr body) =
    Let state pat expr (modifyContinue targets body)
modifyContinue _ expr = expr

removeLoopArgs :: (Monad m) => KExpr -> OptimStateT m KExpr
removeLoopArgs (Loop state args values body) =
    pure $ Loop state newArgs newValues newBody
  where
    toBeRemoved = reused $ execState (visitLoopBody body) $ LoopArgsContext args $ fromList args
    mapping = filter (\(a, _) -> a `member` toBeRemoved) $ zip args values
    (newArgs, newValues) = unzip $ filter (\(a, _) -> a `notMember` toBeRemoved) $ zip args values
    modifiedBody = modifyContinue toBeRemoved body
    newBody = foldl (\body' (from, to) -> subst from to body') modifiedBody mapping
removeLoopArgs (If state cond then' else') = do
    then'' <- removeLoopArgs then'
    else'' <- removeLoopArgs else'
    pure $ If state cond then'' else''
removeLoopArgs (Let state pat expr body) = do
    expr' <- removeLoopArgs expr
    body' <- removeLoopArgs body
    pure $ Let state pat expr' body'
removeLoopArgs expr = pure expr
