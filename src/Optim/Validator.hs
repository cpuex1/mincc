{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Optim.Validator (validate, validateFlatten, validateType) where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Trans (MonadTrans (lift))
import Display (Display (display))
import Error (CompilerError (AssertionError))
import IdentAnalysis (IdentEnvT, getTyOf)
import Syntax (
    AllowCompBranch,
    BinaryOp (FloatOp, IntOp, RelationOp),
    Cond (..),
    Expr (..),
    Ident,
    KExpr,
    Loc,
    Pattern (PRec, PTuple, PUnit, PVar),
    TypedState (TypedState, getType),
    UnaryOp (FNeg, Neg, Not),
    getExprState,
    getLiteralType,
 )
import Typing (Ty, TypeKind (TArray, TBool, TFloat, TFun, TInt, TTuple, TUnit))

type Validator m = ExceptT CompilerError (IdentEnvT m)

validate :: (Monad m) => KExpr -> IdentEnvT m (Either CompilerError ())
validate expr = do
    runExceptT $ do
        validateFlatten expr
        validateType expr

validateFlatten :: (Monad m) => KExpr -> Validator m ()
validateFlatten (Let _ (PRec _ _) expr body) = do
    validateFlatten expr
    validateFlatten body
validateFlatten (Let (TypedState _ loc) _ (Let{}) _) = do
    throwError $ AssertionError loc "The expression is not flatten."
validateFlatten (Let _ _ expr body) = do
    validateFlatten expr
    validateFlatten body
validateFlatten (If _ _ lhs rhs) = do
    validateFlatten lhs
    validateFlatten rhs
validateFlatten _ = pure ()

checkTy :: (Monad m) => Loc -> Ty -> Ty -> Validator m ()
checkTy loc expected actual = do
    unless (expected == actual) $
        throwError $
            AssertionError loc $
                "Expected " <> display expected <> " but got " <> display actual

validateType :: (Monad m) => KExpr -> Validator m ()
validateType (Const (TypedState ty loc) lit) =
    checkTy loc ty litTy
  where
    litTy = getLiteralType lit
validateType (Unary (TypedState ty loc) op operand) = do
    operandTy <- lift $ getTyOf operand
    case op of
        Not -> do
            checkTy loc TBool ty
            checkTy loc TBool operandTy
        Neg -> do
            checkTy loc TInt ty
            checkTy loc TInt operandTy
        FNeg -> do
            checkTy loc TFloat ty
            checkTy loc TFloat operandTy
validateType (Binary (TypedState ty loc) op lhs rhs) = do
    lhsTy <- lift $ getTyOf lhs
    rhsTy <- lift $ getTyOf rhs
    case op of
        RelationOp _ -> do
            checkTy loc TBool ty
            checkTy loc lhsTy rhsTy
        IntOp _ -> do
            checkTy loc TInt ty
            checkTy loc TInt lhsTy
            checkTy loc TInt rhsTy
        FloatOp _ -> do
            checkTy loc TFloat ty
            checkTy loc TFloat lhsTy
            checkTy loc TFloat rhsTy
validateType (If (TypedState ty loc) cond lhs rhs) = do
    validateCond cond
    validateType lhs
    validateType rhs
    checkTy loc ty lhsTy
    checkTy loc ty rhsTy
  where
    lhsTy = getType $ getExprState lhs
    rhsTy = getType $ getExprState rhs

    validateCond :: (Monad m) => Cond Ident AllowCompBranch -> Validator m ()
    validateCond (CIdentity ident) = do
        identTy <- lift $ getTyOf ident
        checkTy loc TBool identTy
    validateCond (CComp _ lhs' rhs') = do
        lhsTy' <- lift $ getTyOf lhs'
        rhsTy' <- lift $ getTyOf rhs'
        checkTy loc lhsTy' rhsTy'
validateType (Let (TypedState ty loc) PUnit expr body) = do
    checkTy loc TUnit exprTy
    checkTy loc ty bodyTy
    validateType expr
    validateType body
  where
    exprTy = getType $ getExprState expr
    bodyTy = getType $ getExprState body
validateType (Let (TypedState ty loc) (PVar v) expr body) = do
    vTy <- lift $ getTyOf v
    checkTy loc vTy exprTy
    checkTy loc ty bodyTy
    validateType expr
    validateType body
  where
    exprTy = getType $ getExprState expr
    bodyTy = getType $ getExprState body
validateType (Let (TypedState ty loc) (PTuple vals) expr body) = do
    vTy <- lift $ TTuple <$> mapM getTyOf vals
    checkTy loc vTy exprTy
    checkTy loc ty bodyTy
    validateType expr
    validateType body
  where
    exprTy = getType $ getExprState expr
    bodyTy = getType $ getExprState body
validateType (Let (TypedState ty loc) (PRec func args) expr body) = do
    funcTy <- lift $ getTyOf func
    funcTy' <- lift $ flip TFun exprTy <$> mapM getTyOf args
    checkTy loc funcTy funcTy'
    checkTy loc ty bodyTy
    validateType expr
    validateType body
  where
    exprTy = getType $ getExprState expr
    bodyTy = getType $ getExprState body
validateType (Var (TypedState ty loc) v) = do
    vTy <- lift $ getTyOf v
    checkTy loc ty vTy
validateType (App (TypedState ty loc) func args) = do
    funcTy <- lift $ getTyOf func
    funcTy' <- lift $ flip TFun ty <$> mapM getTyOf args
    checkTy loc funcTy funcTy'
validateType (Tuple (TypedState ty loc) vals) = do
    vTy <- lift $ TTuple <$> mapM getTyOf vals
    checkTy loc ty vTy
validateType (ArrayCreate (TypedState ty loc) size val) = do
    sizeTy <- lift $ getTyOf size
    arrTy <- lift $ TArray <$> getTyOf val
    checkTy loc TInt sizeTy
    checkTy loc ty arrTy
validateType (Get (TypedState ty loc) arr idx) = do
    arrTy <- lift $ getTyOf arr
    idxTy <- lift $ getTyOf idx
    checkTy loc (TArray ty) arrTy
    checkTy loc TInt idxTy
validateType (Put (TypedState ty loc) arr idx val) = do
    arrTy <- lift $ getTyOf arr
    idxTy <- lift $ getTyOf idx
    valTy <- lift $ getTyOf val
    checkTy loc TUnit ty
    checkTy loc TInt idxTy
    checkTy loc arrTy (TArray valTy)
