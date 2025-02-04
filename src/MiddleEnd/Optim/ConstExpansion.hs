{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MiddleEnd.Optim.ConstExpansion (expandConstants) where

import Control.Monad (foldM)
import Control.Monad.Trans (MonadTrans (lift))
import Data.List (union)
import FrontEnd.Flatten (flattenExpr)
import MiddleEnd.Analysis.Constant (registerConstants)
import MiddleEnd.Analysis.Identifier (IdentProp (constant), genNewVar, getTyOf, searchProp)
import MiddleEnd.Optim.Common (OptimStateT)
import Syntax (
    Cond (CComp, CIdentity),
    Expr (..),
    Ident,
    KExpr,
    Literal,
    Pattern (PRec, PVar),
    TypedState (TypedState, getType),
    dummyLoc,
    getExprState,
    subst,
 )

-- | Picks a variable if and only if it is a constant.
pickConstant :: (Monad m) => Ident -> OptimStateT m [(Ident, Literal)]
pickConstant ident = do
    prop <- lift $ searchProp ident
    case prop of
        Just prop' ->
            case constant prop' of
                Just lit ->
                    pure [(ident, lit)]
                Nothing -> pure []
        Nothing -> pure []

-- | Finds all required constants from the function.
requiredConstants :: (Monad m) => KExpr -> OptimStateT m [(Ident, Literal)]
requiredConstants (Const _ _) = pure []
requiredConstants (Unary _ _ ident) = pickConstant ident
requiredConstants (Binary _ _ ident1 ident2) = do
    c1 <- pickConstant ident1
    c2 <- pickConstant ident2
    pure $ c1 `union` c2
requiredConstants (If _ cond t f) = do
    cond' <- pickConstantInCond cond
    t' <- requiredConstants t
    f' <- requiredConstants f
    pure $ cond' `union` t' `union` f'
  where
    pickConstantInCond :: (Monad m) => Cond Ident True -> OptimStateT m [(Ident, Literal)]
    pickConstantInCond (CIdentity cond') = pickConstant cond'
    pickConstantInCond (CComp _ lhs rhs) = do
        lhs' <- pickConstant lhs
        rhs' <- pickConstant rhs
        pure $ lhs' `union` rhs'
requiredConstants (Let _ (PRec _ _) _ body) =
    -- The `let rec` pattern creates a new function, which has a different scope.
    requiredConstants body
requiredConstants (Let _ _ expr body) = do
    expr' <- requiredConstants expr
    body' <- requiredConstants body
    pure $ expr' `union` body'
requiredConstants (Var _ v) = pickConstant v
requiredConstants (App _ _ args) =
    foldl union [] <$> mapM pickConstant args
requiredConstants (Tuple _ args) =
    foldl union [] <$> mapM pickConstant args
requiredConstants (ArrayCreate _ size val) = do
    size' <- pickConstant size
    val' <- pickConstant val
    pure $ size' `union` val'
requiredConstants (Get _ _ idx) =
    pickConstant idx
requiredConstants (Put _ _ idx val) = do
    idx' <- pickConstant idx
    val' <- pickConstant val
    pure $ idx' `union` val'

expandChildrenConstants :: (Monad m) => KExpr -> OptimStateT m KExpr
expandChildrenConstants (Let state (PRec v pats) expr body) = do
    -- If the function was found, just expand all constants in that body.
    expr' <- expandConstants' expr
    body' <- expandChildrenConstants body
    pure $ Let state (PRec v pats) expr' body'
expandChildrenConstants (Let state pattern expr body) = do
    expr' <- expandChildrenConstants expr
    body' <- expandChildrenConstants body
    pure $ Let state pattern expr' body'
expandChildrenConstants (If state cond t f) = do
    t' <- expandChildrenConstants t
    f' <- expandChildrenConstants f
    pure $ If state cond t' f'
expandChildrenConstants expr = pure expr

-- | Expands all constants in the expression.
expandConstants :: (Monad m) => KExpr -> OptimStateT m KExpr
expandConstants expr = do
    lift $ registerConstants expr
    expr' <- expandChildrenConstants expr
    let expr'' = flattenExpr expr'
    lift $ registerConstants expr''
    pure expr''

expandConstants' :: (Monad m) => KExpr -> OptimStateT m KExpr
expandConstants' expr = do
    expr' <- expandChildrenConstants expr
    consts <- requiredConstants expr'
    foldM
        ( \body (ident, lit) -> do
            let exprTy = getType $ getExprState body
            ty <- lift $ getTyOf ident
            v <- lift $ genNewVar ty
            let constExpr = Const (TypedState ty dummyLoc) lit
            let substBody = subst ident v body
            pure $ Let (TypedState exprTy dummyLoc) (PVar v) constExpr substBody
        )
        expr'
        consts
