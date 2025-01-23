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
    TypedState (TypedState, getLoc),
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
runCSE expr = evalStateT (commonSubexprElim expr) $ CSEContext mempty

-- | Eliminates common subexpressions.
commonSubexprElim :: (Monad m) => KExpr -> CSEStateT m KExpr
commonSubexprElim (Let state (PVar v) expr body) = do
    ctx <- get
    modifiedExpr <- commonSubexprElim expr
    put ctx
    case modifiedExpr of
        Var _ _ -> do
            -- Can be replaced with the variable by removing common subexpressions.
            modifiedBody <- commonSubexprElim body
            pure $ Let state (PVar v) modifiedExpr modifiedBody
        _ -> do
            if definitelyPure modifiedExpr
                then do
                    -- The expression is pure, so we can bind the expression to the variable.
                    let purged = purge modifiedExpr
                    modify $ \ctx' -> ctx'{variableMap = insert purged v $ variableMap ctx}
                    modifiedBody <- commonSubexprElim body
                    pure $ Let state (PVar v) modifiedExpr modifiedBody
                else do
                    -- The expression is not pure.
                    modifiedBody <- commonSubexprElim body
                    pure $ Let state (PVar v) modifiedExpr modifiedBody
commonSubexprElim (Let state (PRec func args) expr body) = do
    -- The function body should be handled within a empty context.
    ctx <- get
    put $ CSEContext mempty
    modifiedExpr <- commonSubexprElim expr
    put ctx
    modifiedBody <- commonSubexprElim body
    pure $ Let state (PRec func args) modifiedExpr modifiedBody
commonSubexprElim (Let state pattern expr body) = do
    ctx <- get
    modifiedExpr <- commonSubexprElim expr
    put ctx
    modifiedBody <- commonSubexprElim body
    pure $ Let state pattern modifiedExpr modifiedBody
commonSubexprElim (If state cond then' else') = do
    ctx <- get
    modifiedThen <- commonSubexprElim then'
    put ctx
    modifiedElse <- commonSubexprElim else'
    pure $ If state cond modifiedThen modifiedElse
commonSubexprElim expr = do
    searched <- gets $ lookup purged . variableMap
    case searched of
        Just v -> do
            -- The expression is already bounded.
            ty <- lift $ lift $ getTyOf v
            pure $ Var (TypedState ty loc) v
        Nothing ->
            -- The expression has not been appeared yet.
            pure expr
  where
    purged = purge expr
    loc = getLoc $ getExprState expr
