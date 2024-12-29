{-# LANGUAGE GADTs #-}

module Optim.ConstFold (constFold) where

import ConstantAnalysis (registerConstants)
import Control.Monad.Trans (MonadTrans (lift))
import IdentAnalysis (asConstant)
import Optim.Base (OptimStateT)
import Syntax (
    BinaryOp (FloatOp, IntOp, RelationOp),
    Cond (CComp, CIdentity),
    Expr (..),
    FloatBinOp (FAdd, FDiv, FMul, FSub),
    IntBinOp (Add, Div, Mul, Sub),
    KExpr,
    Literal (LBool, LFloat, LInt),
    RelationBinOp (Eq, Ge, Lt, Ne),
    UnaryOp (FNeg, Neg, Not),
 )

performRelationOp :: (Ord a) => RelationBinOp -> a -> a -> Bool
performRelationOp Eq lhs' rhs' = lhs' == rhs'
performRelationOp Ne lhs' rhs' = lhs' /= rhs'
performRelationOp Lt lhs' rhs' = lhs' < rhs'
performRelationOp Ge lhs' rhs' = lhs' <= rhs'

-- | Calculates constant variables in the compile-time.
constFold :: (Monad m) => KExpr -> OptimStateT m KExpr
constFold expr = do
    lift $ registerConstants expr
    expr' <- constFold' expr
    lift $ registerConstants expr'
    pure expr'

constFold' :: (Monad m) => KExpr -> OptimStateT m KExpr
constFold' (Const state lit) = pure $ Const state lit
constFold' (Unary state Not ident) = do
    c <- lift $ asConstant ident
    case c of
        Just (LBool b) ->
            pure $ Const state (LBool $ not b)
        _ ->
            pure $ Unary state Not ident
constFold' (Unary state Neg ident) = do
    c <- lift $ asConstant ident
    case c of
        Just (LInt i) ->
            pure $ Const state (LInt (-i))
        _ ->
            pure $ Unary state Neg ident
constFold' (Unary state FNeg ident) = do
    c <- lift $ asConstant ident
    case c of
        Just (LFloat i) ->
            pure $ Const state (LFloat (-i))
        _ ->
            pure $ Unary state FNeg ident
constFold' (Binary state (RelationOp op) lhs rhs) = do
    lhs' <- lift $ asConstant lhs
    rhs' <- lift $ asConstant rhs
    case (lhs', rhs') of
        (Just (LInt lhs''), Just (LInt rhs'')) ->
            pure $ Const state (LBool $ performRelationOp op lhs'' rhs'')
        (Just (LFloat lhs''), Just (LFloat rhs'')) ->
            pure $ Const state (LBool $ performRelationOp op lhs'' rhs'')
        _ ->
            pure $ Binary state (RelationOp op) lhs rhs
constFold' (Binary state (IntOp op) lhs rhs) = do
    lhs' <- lift $ asConstant lhs
    rhs' <- lift $ asConstant rhs
    case (lhs', rhs') of
        (Just (LInt lhs''), Just (LInt rhs'')) ->
            pure $ Const state (LInt $ performOp op lhs'' rhs'')
        _ ->
            pure $ Binary state (IntOp op) lhs rhs
  where
    performOp :: IntBinOp -> Int -> Int -> Int
    performOp Add lhs' rhs' = lhs' + rhs'
    performOp Sub lhs' rhs' = lhs' - rhs'
    performOp Mul lhs' rhs' = lhs' * rhs'
    performOp Div lhs' rhs' = lhs' `div` rhs'
constFold' (Binary state (FloatOp op) lhs rhs) = do
    lhs' <- lift $ asConstant lhs
    rhs' <- lift $ asConstant rhs
    case (lhs', rhs') of
        (Just (LFloat lhs''), Just (LFloat rhs'')) ->
            pure $ Const state (LFloat $ performOp op lhs'' rhs'')
        _ ->
            pure $ Binary state (FloatOp op) lhs rhs
  where
    performOp :: FloatBinOp -> Float -> Float -> Float
    performOp FAdd lhs' rhs' = lhs' + rhs'
    performOp FSub lhs' rhs' = lhs' - rhs'
    performOp FMul lhs' rhs' = lhs' * rhs'
    performOp FDiv lhs' rhs' = lhs' / rhs'
constFold' (If state (CIdentity cond) t f) = do
    cond' <- lift $ asConstant cond
    case cond' of
        Just (LBool True) -> constFold' t
        Just (LBool False) -> constFold' f
        _ -> do
            t' <- constFold' t
            f' <- constFold' f
            pure $ If state (CIdentity cond) t' f'
constFold' (If state (CComp op lhs rhs) t f) = do
    lhs' <- lift $ asConstant lhs
    rhs' <- lift $ asConstant rhs
    case (lhs', rhs') of
        (Just (LInt lhs''), Just (LInt rhs'')) ->
            pure $ if performRelationOp op lhs'' rhs'' then t else f
        (Just (LFloat lhs''), Just (LFloat rhs'')) ->
            pure $ if performRelationOp op lhs'' rhs'' then t else f
        _ -> do
            t' <- constFold' t
            f' <- constFold' f
            pure $ If state (CComp op lhs rhs) t' f'
constFold' (Let state pattern expr body) = do
    expr' <- constFold' expr
    body' <- constFold' body
    pure $ Let state pattern expr' body'
constFold' (Var state v) = do
    v' <- lift $ asConstant v
    case v' of
        Just lit -> pure $ Const state lit
        _ -> pure $ Var state v
constFold' (App state v args) = pure $ App state v args
constFold' (Tuple state args) = pure $ Tuple state args
constFold' (ArrayCreate state size val) = pure $ ArrayCreate state size val
constFold' (Get state arr idx) = pure $ Get state arr idx
constFold' (Put state arr idx val) = pure $ Put state arr idx val
