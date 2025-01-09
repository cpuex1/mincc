{-# LANGUAGE GADTs #-}

module MiddleEnd.Optim.Common (
    genFresh,
    withFreshVars,
    Threshold (..),
    toThreshold,
    OptimContext (..),
    OptimStateT,
) where

import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import MiddleEnd.Analysis.Identifier (IdentEnvT, genNewVar, getTyOf)
import Syntax (Expr (..), Ident, KExpr, Pattern (PRec, PTuple, PUnit, PVar), subst)

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
