{-# LANGUAGE GADTs #-}

module MiddleEnd.Optim.UnitElim (elimUnitArgs) where

import Control.Monad (filterM)
import Control.Monad.Trans (MonadTrans (lift))
import MiddleEnd.Analysis.Identifier (IdentProp (typeOf), genNewVar, getTyOf, updateProp)
import MiddleEnd.Optim.Base (OptimStateT)
import Syntax (Expr (App, Const, If, Let), Ident (ExternalIdent), KExpr, Literal (LUnit), Pattern (PRec, PVar), TypedState (TypedState, getType), dummyLoc, getExprState, subst)
import Typing (TypeKind (TFun, TUnit))

-- | Removes arguments with unit types by modifying function declarations.
elimUnitArgs :: (Monad m) => KExpr -> OptimStateT m KExpr
elimUnitArgs (Let state (PRec f args) value body) = do
    unitRemovedTypes <- lift $ filter (/= TUnit) <$> mapM getTyOf args
    let valueTy = getType (getExprState value)
    let bodyTy = getType (getExprState body)

    unitArgs <- lift $ filterM (fmap (== TUnit) . getTyOf) args
    nonUnitArgs <- lift $ filterM (fmap (/= TUnit) . getTyOf) args
    valueWithUnit <-
        if null unitArgs
            then
                pure value
            else do
                u <- lift $ genNewVar TUnit
                let value' = foldl (\e arg -> subst arg u arg u e) value unitArgs
                pure (Let (TypedState bodyTy dummyLoc) (PVar u) (Const (TypedState TUnit dummyLoc) LUnit) value')
    lift $ updateProp f (\prop -> prop{typeOf = TFun unitRemovedTypes valueTy})

    elimValue <- elimUnitArgs valueWithUnit
    elimBody <- elimUnitArgs body
    pure $ Let state (PRec f nonUnitArgs) elimValue elimBody
elimUnitArgs (Let state pat value body) = do
    value' <- elimUnitArgs value
    body' <- elimUnitArgs body
    pure $ Let state pat value' body'
elimUnitArgs (If state cond thenE elseE) = do
    thenE' <- elimUnitArgs thenE
    elseE' <- elimUnitArgs elseE
    pure $ If state cond thenE' elseE'
elimUnitArgs (App state (ExternalIdent ext) args) = do
    nonUnitArgs <- lift $ filterM (fmap (/= TUnit) . getTyOf) args

    -- Update external identifier's type.
    argTypes <- lift $ mapM getTyOf nonUnitArgs
    let retTy = getType state
    lift $ updateProp (ExternalIdent ext) (\prop -> prop{typeOf = TFun argTypes retTy})

    pure $ App state (ExternalIdent ext) nonUnitArgs
elimUnitArgs (App state func args) = do
    nonUnitArgs <- lift $ filterM (fmap (/= TUnit) . getTyOf) args
    pure $ App state func nonUnitArgs
elimUnitArgs expr = pure expr
