{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Optim.Validator (validate, validateFlatten, validateType) where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Text (Text)
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

checkTy :: (Monad m) => Loc -> Ty -> Ty -> Text -> Validator m ()
checkTy loc expected actual msg = do
    unless (expected == actual) $
        throwError $
            AssertionError loc $
                "Expected " <> display expected <> " but got " <> display actual <> " (info: " <> msg <> ")"

validateType :: (Monad m) => KExpr -> Validator m ()
validateType (Const (TypedState ty loc) lit) =
    checkTy loc ty litTy "const"
  where
    litTy = getLiteralType lit
validateType (Unary (TypedState ty loc) op operand) = do
    operandTy <- lift $ getTyOf operand
    case op of
        Not -> do
            checkTy loc TBool ty "unary not1"
            checkTy loc TBool operandTy "unary not2"
        Neg -> do
            checkTy loc TInt ty "unary neg1"
            checkTy loc TInt operandTy "unary neg2"
        FNeg -> do
            checkTy loc TFloat ty "unary fneg1"
            checkTy loc TFloat operandTy "unary fneg2"
validateType (Binary (TypedState ty loc) op lhs rhs) = do
    lhsTy <- lift $ getTyOf lhs
    rhsTy <- lift $ getTyOf rhs
    case op of
        RelationOp _ -> do
            checkTy loc TBool ty "binary relation1"
            checkTy loc lhsTy rhsTy "binary relation2"
        IntOp _ -> do
            checkTy loc TInt ty "binary int1"
            checkTy loc TInt lhsTy "binary int2"
            checkTy loc TInt rhsTy "binary int3"
        FloatOp _ -> do
            checkTy loc TFloat ty "binary float1"
            checkTy loc TFloat lhsTy "binary float2"
            checkTy loc TFloat rhsTy "binary float3"
validateType (If (TypedState ty loc) cond lhs rhs) = do
    validateCond cond
    validateType lhs
    validateType rhs
    checkTy loc ty lhsTy "if1"
    checkTy loc ty rhsTy "if2"
  where
    lhsTy = getType $ getExprState lhs
    rhsTy = getType $ getExprState rhs

    validateCond :: (Monad m) => Cond Ident AllowCompBranch -> Validator m ()
    validateCond (CIdentity ident) = do
        identTy <- lift $ getTyOf ident
        checkTy loc TBool identTy "if3"
    validateCond (CComp _ lhs' rhs') = do
        lhsTy' <- lift $ getTyOf lhs'
        rhsTy' <- lift $ getTyOf rhs'
        checkTy loc lhsTy' rhsTy' "if4"
validateType (Let (TypedState ty loc) PUnit expr body) = do
    checkTy loc TUnit exprTy "let unit1"
    checkTy loc ty bodyTy "let unit2"
    validateType expr
    validateType body
  where
    exprTy = getType $ getExprState expr
    bodyTy = getType $ getExprState body
validateType (Let (TypedState ty loc) (PVar v) expr body) = do
    vTy <- lift $ getTyOf v
    checkTy loc vTy exprTy "let var1"
    checkTy loc ty bodyTy "let var2"
    validateType expr
    validateType body
  where
    exprTy = getType $ getExprState expr
    bodyTy = getType $ getExprState body
validateType (Let (TypedState ty loc) (PTuple vals) expr body) = do
    vTy <- lift $ TTuple <$> mapM getTyOf vals
    checkTy loc vTy exprTy "let tuple1"
    checkTy loc ty bodyTy "let tuple2"
    validateType expr
    validateType body
  where
    exprTy = getType $ getExprState expr
    bodyTy = getType $ getExprState body
validateType (Let (TypedState ty loc) (PRec func args) expr body) = do
    funcTy <- lift $ getTyOf func
    funcTy' <- lift $ flip TFun exprTy <$> mapM getTyOf args
    checkTy loc funcTy funcTy' "let rec1"
    checkTy loc ty bodyTy "let rec2"
    validateType expr
    validateType body
  where
    exprTy = getType $ getExprState expr
    bodyTy = getType $ getExprState body
validateType (Var (TypedState ty loc) v) = do
    vTy <- lift $ getTyOf v
    checkTy loc ty vTy "var"
validateType (App (TypedState ty loc) func args) = do
    funcTy <- lift $ getTyOf func
    funcTy' <- lift $ flip TFun ty <$> mapM getTyOf args
    checkTy loc funcTy funcTy' "app"
validateType (Tuple (TypedState ty loc) vals) = do
    vTy <- lift $ TTuple <$> mapM getTyOf vals
    checkTy loc ty vTy "tuple"
validateType (ArrayCreate (TypedState ty loc) size val) = do
    sizeTy <- lift $ getTyOf size
    arrTy <- lift $ TArray <$> getTyOf val
    checkTy loc TInt sizeTy "array create1"
    checkTy loc ty arrTy "array create2"
validateType (Get (TypedState ty loc) arr idx) = do
    arrTy <- lift $ getTyOf arr
    idxTy <- lift $ getTyOf idx
    checkTy loc (TArray ty) arrTy "get1"
    checkTy loc TInt idxTy "get2"
validateType (Put (TypedState ty loc) arr idx val) = do
    arrTy <- lift $ getTyOf arr
    idxTy <- lift $ getTyOf idx
    valTy <- lift $ getTyOf val
    checkTy loc TUnit ty "put1"
    checkTy loc TInt idxTy "put2"
    checkTy loc arrTy (TArray valTy) "put3"
