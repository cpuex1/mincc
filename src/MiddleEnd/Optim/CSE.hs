{-# LANGUAGE GADTs #-}

module MiddleEnd.Optim.CSE (
    runCSE,
) where

import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT, evalStateT, gets, modify)
import Data.Map (Map, insert, lookup)
import MiddleEnd.Analysis.Common (definitelyPure)
import MiddleEnd.Analysis.Identifier (getTyOf)
import MiddleEnd.Optim.Common (OptimStateT, purge)
import Syntax (
    Expr (..),
    Ident,
    KExpr,
    Pattern (PRec, PVar),
    TState (TState, getLoc),
    getExprState,
 )
import Prelude hiding (lookup)

-- | Holds pure and purged expressions that is already bounded.
newtype CSEContext = CSEContext
    { variableMap :: Map KExpr Ident
    }
    deriving (Show, Eq)

type CSEStateT m = StateT CSEContext (OptimStateT m)

runCSE :: (Monad m) => KExpr -> OptimStateT m KExpr
runCSE expr = evalStateT (commonSElim expr) $ CSEContext mempty

-- | Eliminates common subexpressions.
commonSElim :: (Monad m) => KExpr -> CSEStateT m KExpr
commonSElim (Let state (PVar v) expr body) = do
    ctx <- get
    modifiedExpr <- commonSElim expr
    put ctx
    case modifiedExpr of
        Var _ _ -> do
            -- Can be replaced with the variable by removing common subexpressions.
            modifiedBody <- commonSElim body
            pure $ Let state (PVar v) modifiedExpr modifiedBody
        _ -> do
            if definitelyPure modifiedExpr
                then do
                    -- The expression is pure, so we can bind the expression to the variable.
                    let purged = purge modifiedExpr
                    modify $ \ctx' -> ctx'{variableMap = insert purged v $ variableMap ctx}
                    modifiedBody <- commonSElim body
                    pure $ Let state (PVar v) modifiedExpr modifiedBody
                else do
                    -- The expression is not pure.
                    modifiedBody <- commonSElim body
                    pure $ Let state (PVar v) modifiedExpr modifiedBody
commonSElim (Let state (PRec func args) expr body) = do
    -- The function body should be handled within a empty context.
    ctx <- get
    put $ CSEContext mempty
    modifiedExpr <- commonSElim expr
    put ctx
    modifiedBody <- commonSElim body
    pure $ Let state (PRec func args) modifiedExpr modifiedBody
commonSElim (Let state pattern expr body) = do
    ctx <- get
    modifiedExpr <- commonSElim expr
    put ctx
    modifiedBody <- commonSElim body
    pure $ Let state pattern modifiedExpr modifiedBody
commonSElim (If state cond then' else') = do
    ctx <- get
    modifiedThen <- commonSElim then'
    put ctx
    modifiedElse <- commonSElim else'
    pure $ If state cond modifiedThen modifiedElse
commonSElim (Loop state args values body) = do
    ctx <- get
    modifiedBody <- commonSElim body
    put ctx
    pure $ Loop state args values modifiedBody
commonSElim expr = do
    searched <- gets $ lookup purged . variableMap
    case searched of
        Just v -> do
            -- The expression is already bounded.
            ty <- lift $ lift $ getTyOf v
            pure $ Var (TState ty loc) v
        Nothing ->
            -- The expression has not been appeared yet.
            pure expr
  where
    purged = purge expr
    loc = getLoc $ getExprState expr
