{-# LANGUAGE GADTs #-}

module Optim.Base (
    definitelyPure,
    exprSize,
    isUsed,
    withFreshVars,
    OptimContext (..),
    OptimStateT,
    Threshold (..),
    toThreshold,
) where

import Control.Monad.State (State, StateT, execState, modify)
import Control.Monad.Trans (lift)
import IdentAnalysis (IdentEnvT, genNewVar, getTyOf)
import Syntax (Expr (..), Ident, KExpr, Pattern (PRec, PTuple, PUnit, PVar), subst, visitExprM)

{- | Checks if the expression is pure.
This analysis is too conservative and can return false negatives.
-}
definitelyPure :: KExpr -> Bool
definitelyPure (Const{}) = True
definitelyPure (Unary{}) = True
definitelyPure (Binary{}) = True
definitelyPure (If _ _ then' else') = definitelyPure then' && definitelyPure else'
definitelyPure (Let _ _ expr body) = definitelyPure expr && definitelyPure body
definitelyPure (Var{}) = True
definitelyPure (App{}) = False
definitelyPure (Tuple{}) = True
definitelyPure (ArrayCreate{}) = False
definitelyPure (Get{}) =
    -- A global array can be modified in another function.
    False
definitelyPure (Put{}) = False

-- | Calculates the size of the expression.
exprSize :: KExpr -> Int
exprSize (Const{}) = 1
exprSize (Unary{}) = 1
exprSize (Binary{}) = 1
exprSize (If _ _ then' else') = 1 + exprSize then' + exprSize else'
exprSize (Let _ _ expr body) = 1 + exprSize expr + exprSize body
exprSize (Var _ _) = 1
exprSize (App _ _ args) = 1 + length args
exprSize (Tuple _ args) = 1 + length args
exprSize (ArrayCreate{}) = 1
exprSize (Get{}) = 1
exprSize (Put{}) = 1

-- | Checks if the identifier is used in the expression.
isUsed :: Ident -> KExpr -> Bool
isUsed ident expr = execState isUsed' False
  where
    tryMarkUsed :: Ident -> State Bool ()
    tryMarkUsed ident' = modify (\used -> used || ident' == ident)

    isUsed' :: State Bool KExpr
    isUsed' = do
        visitExprM
            pure
            (\i -> tryMarkUsed i >> pure i)
            (\i -> tryMarkUsed i >> pure i)
            expr

-- | Generates a new variable conditionally.
genFresh :: (Monad m) => Ident -> OptimStateT m Ident
genFresh ident = do
    ty <- lift $ getTyOf ident
    lift $ genNewVar ty

{- | Replaces bounded variables with fresh ones.
This function is implemented to keep all variable names unique.
-}
withFreshVars :: (Monad m) => KExpr -> OptimStateT m KExpr
withFreshVars (Let state (PVar ident) expr body) = do
    fresh <- genFresh ident
    expr' <- withFreshVars expr
    body' <- withFreshVars $ subst ident fresh ident fresh body
    pure $ Let state (PVar fresh) expr' body'
withFreshVars (Let state (PRec func args) expr body) = do
    freshFunc <- genFresh func
    freshArgs <- mapM genFresh args
    expr' <-
        withFreshVars
            $ foldl
                (\e (from, to) -> subst from to from to e)
                expr
            $ zip (func : args) (freshFunc : freshArgs)
    body' <- withFreshVars $ subst func freshFunc func freshFunc body
    pure $ Let state (PRec freshFunc freshArgs) expr' body'
withFreshVars (Let state (PTuple values) expr body) = do
    freshValues <- mapM genFresh values
    expr' <- withFreshVars expr
    body' <-
        withFreshVars
            $ foldl
                (\e (from, to) -> subst from to from to e)
                body
            $ zip values freshValues
    pure $ Let state (PTuple freshValues) expr' body'
withFreshVars (Let state PUnit expr body) = do
    expr' <- withFreshVars expr
    body' <- withFreshVars body
    pure $ Let state PUnit expr' body'
withFreshVars (If state cond then' else') = do
    then'' <- withFreshVars then'
    else'' <- withFreshVars else'
    pure $ If state cond then'' else''
withFreshVars expr = pure expr

-- | Threshold for inlining.
data Threshold
    = ThresholdInt Int
    | ThresholdInfinity
    deriving (Show, Eq)

instance Ord Threshold where
    compare (ThresholdInt a) (ThresholdInt b) = compare a b
    compare ThresholdInfinity ThresholdInfinity = EQ
    compare ThresholdInfinity _ = GT
    compare _ ThresholdInfinity = LT

-- | Converts `Maybe Int` to `Threshold`.
toThreshold :: Maybe Int -> Threshold
toThreshold Nothing = ThresholdInfinity
toThreshold (Just n) = ThresholdInt n

data OptimContext = OptimContext
    { inliningSizeThreshold :: Threshold
    , recInliningLimit :: Int
    , recInliningSizeThreshold :: Threshold
    }
    deriving (Show, Eq)

type OptimStateT m = StateT OptimContext (IdentEnvT m)
