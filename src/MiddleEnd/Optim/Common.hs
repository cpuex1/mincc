{-# LANGUAGE GADTs #-}

module MiddleEnd.Optim.Common (
    genFresh,
    withFreshVars,
    purge,
    occur,
    Threshold (..),
    toThreshold,
    OptimContext (..),
    OptimStateT,
) where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import MiddleEnd.Analysis.Identifier (IdentEnvT, genNewVar, getTyOf)
import Syntax (
    Cond (CComp, CIdentity),
    Expr (..),
    Ident,
    KExpr,
    Pattern (PRec, PTuple, PUnit, PVar),
    TypedState (getLoc),
    dummyLoc,
    subst,
    visitExprM,
 )

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
    body' <- withFreshVars $ subst ident fresh body
    pure $ Let state (PVar fresh) expr' body'
withFreshVars (Let state (PRec func args) expr body) = do
    freshFunc <- genFresh func
    freshArgs <- mapM genFresh args
    expr' <-
        withFreshVars
            $ foldl
                (\e (from, to) -> subst from to e)
                expr
            $ zip (func : args) (freshFunc : freshArgs)
    body' <- withFreshVars $ subst func freshFunc body
    pure $ Let state (PRec freshFunc freshArgs) expr' body'
withFreshVars (Let state (PTuple values) expr body) = do
    freshValues <- mapM genFresh values
    expr' <- withFreshVars expr
    body' <-
        withFreshVars
            $ foldl
                (\e (from, to) -> subst from to e)
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
withFreshVars (Loop state args values body) = do
    freshArgs <- mapM genFresh args
    body' <-
        withFreshVars
            $ foldl
                (\e (from, to) -> subst from to e)
                body
            $ zip args freshArgs
    pure $ Loop state args values body'
withFreshVars expr = pure expr

-- | Purges all location information.
purge :: KExpr -> KExpr
purge =
    runIdentity
        . visitExprM
            ( \state ->
                pure $ state{getLoc = dummyLoc}
            )
            pure
            pure

occur :: Ident -> KExpr -> Bool
occur _ Const{} = False
occur ident (Unary _ _ expr) = ident == expr
occur ident (Binary _ _ lhs rhs) = ident == lhs || ident == rhs
occur ident (If _ (CIdentity cond) then' else') = ident == cond || occur ident then' || occur ident else'
occur ident (If _ (CComp _ lhs rhs) then' else') = ident == lhs || ident == rhs || occur ident then' || occur ident else'
occur ident (Let _ PUnit expr body) = occur ident expr || occur ident body
occur ident (Let _ (PVar _) expr body) = occur ident expr || occur ident body
occur ident (Let _ (PRec _ _) expr body) = occur ident expr || occur ident body
occur ident (Let _ (PTuple _) expr body) = occur ident expr || occur ident body
occur ident (Var _ ident') = ident == ident'
occur ident (App _ func args) = ident == func || any (== ident) args
occur ident (Tuple _ values) = any (== ident) values
occur ident (ArrayCreate _ size value) = ident == size || ident == value
occur ident (Get _ array index) = ident == array || ident == index
occur ident (Put _ array index value) = ident == array || ident == index || ident == value
occur ident (Loop _ _ values body) = ident `elem` values || occur ident body
occur ident (Continue _ values) = ident `elem` values

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
