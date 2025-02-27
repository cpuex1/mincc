{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module MiddleEnd.Validator (validate, validateFlatten, validateType) where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Text (Text)
import Display (Display (display))
import Error (CompilerError (AssertionError))
import MiddleEnd.Analysis.Identifier (IdentEnvT, getTyOf)
import Syntax (
    BinaryOp (FloatOp, IntOp, RelationOp),
    Cond (..),
    Expr (..),
    Ident,
    KExpr,
    Loc,
    Pattern (PRec, PTuple, PUnit, PVar),
    TState (TState, getType),
    UnaryOp (FNeg, Neg, Not),
    getExprState,
    getLiteralPType,
 )
import Typing (TypeBase (..))

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
validateFlatten (Let (TState _ loc) _ (Let{}) _) = do
    throwError $ AssertionError loc "The expression is not flatten."
validateFlatten (Let _ _ expr body) = do
    validateFlatten expr
    validateFlatten body
validateFlatten (If _ _ lhs rhs) = do
    validateFlatten lhs
    validateFlatten rhs
validateFlatten (Loop _ _ _ body) = do
    validateFlatten body
validateFlatten _ = pure ()

check :: (Monad m, Eq a, Display a) => Loc -> a -> a -> Text -> Validator m ()
check loc expected actual msg = do
    unless (expected == actual) $
        throwError $
            AssertionError loc $
                "Expected " <> display expected <> " but got " <> display actual <> " (info: " <> msg <> ")"

validateType :: (Monad m) => KExpr -> Validator m ()
validateType (Const (TState ty loc) lit) =
    check loc ty litTy "const"
  where
    litTy = getLiteralPType lit
validateType (Unary (TState ty loc) op operand) = do
    operandTy <- lift $ getTyOf operand
    case op of
        Not -> do
            check loc TInt ty "unary not1"
            check loc TInt operandTy "unary not2"
        Neg -> do
            check loc TInt ty "unary neg1"
            check loc TInt operandTy "unary neg2"
        FNeg -> do
            check loc TFloat ty "unary fneg1"
            check loc TFloat operandTy "unary fneg2"
validateType (Binary (TState ty loc) op lhs rhs) = do
    lhsTy <- lift $ getTyOf lhs
    rhsTy <- lift $ getTyOf rhs
    case op of
        RelationOp _ -> do
            check loc TInt ty "binary relation1"
            check loc lhsTy rhsTy "binary relation2"
        IntOp _ -> do
            check loc TInt ty "binary int1"
            check loc TInt lhsTy "binary int2"
            check loc TInt rhsTy "binary int3"
        FloatOp _ -> do
            check loc TFloat ty "binary float1"
            check loc TFloat lhsTy "binary float2"
            check loc TFloat rhsTy "binary float3"
validateType (If (TState ty loc) cond lhs rhs) = do
    validateCond cond
    validateType lhs
    validateType rhs
    check loc ty lhsTy "if1"
    check loc ty rhsTy "if2"
  where
    lhsTy = getType $ getExprState lhs
    rhsTy = getType $ getExprState rhs

    validateCond :: (Monad m) => Cond Ident True -> Validator m ()
    validateCond (CIdentity ident) = do
        identTy <- lift $ getTyOf ident
        check loc TInt identTy "if3"
    validateCond (CNeg ident) = do
        identTy <- lift $ getTyOf ident
        check loc TInt identTy "if4"
    validateCond (CComp _ lhs' rhs') = do
        lhsTy' <- lift $ getTyOf lhs'
        rhsTy' <- lift $ getTyOf rhs'
        check loc lhsTy' rhsTy' "if5"
validateType (Let (TState ty loc) PUnit expr body) = do
    check loc TUnit exprTy "let unit1"
    check loc ty bodyTy "let unit2"
    validateType expr
    validateType body
  where
    exprTy = getType $ getExprState expr
    bodyTy = getType $ getExprState body
validateType (Let (TState ty loc) (PVar v) expr body) = do
    vTy <- lift $ getTyOf v
    check loc vTy exprTy "let var1"
    check loc ty bodyTy "let var2"
    validateType expr
    validateType body
  where
    exprTy = getType $ getExprState expr
    bodyTy = getType $ getExprState body
validateType (Let (TState ty loc) (PTuple vals) expr body) = do
    vTy <- lift $ TTuple <$> mapM getTyOf vals
    check loc vTy exprTy "let tuple1"
    check loc ty bodyTy "let tuple2"
    validateType expr
    validateType body
  where
    exprTy = getType $ getExprState expr
    bodyTy = getType $ getExprState body
validateType (Let (TState ty loc) (PRec func args) expr body) = do
    funcTy <- lift $ getTyOf func
    funcTy' <- lift $ flip TFun exprTy <$> mapM getTyOf args
    check loc funcTy funcTy' "let rec1"
    check loc ty bodyTy "let rec2"
    validateType expr
    validateType body
  where
    exprTy = getType $ getExprState expr
    bodyTy = getType $ getExprState body
validateType (Var (TState ty loc) v) = do
    vTy <- lift $ getTyOf v
    check loc ty vTy "var"
validateType (App (TState ty loc) func args) = do
    funcTy <- lift $ getTyOf func
    funcTy' <- lift $ flip TFun ty <$> mapM getTyOf args
    check loc funcTy funcTy' "app"
validateType (Tuple (TState ty loc) vals) = do
    vTy <- lift $ TTuple <$> mapM getTyOf vals
    check loc ty vTy "tuple"
validateType (ArrayCreate (TState ty loc) size val) = do
    sizeTy <- lift $ getTyOf size
    arrTy <- lift $ TArray <$> getTyOf val
    check loc TInt sizeTy "array create1"
    check loc ty arrTy "array create2"
validateType (Get (TState ty loc) arr idx) = do
    arrTy <- lift $ getTyOf arr
    idxTy <- lift $ getTyOf idx
    check loc (TArray ty) arrTy "get1"
    check loc TInt idxTy "get2"
validateType (Put (TState ty loc) arr idx val) = do
    arrTy <- lift $ getTyOf arr
    idxTy <- lift $ getTyOf idx
    valTy <- lift $ getTyOf val
    check loc TUnit ty "put1"
    check loc TInt idxTy "put2"
    check loc arrTy (TArray valTy) "put3"
validateType (Loop (TState ty loc) args values body) = do
    argsTy <- lift $ mapM getTyOf args
    valuesTy <- lift $ mapM getTyOf values
    check loc (length argsTy) (length valuesTy) "loop1"
    mapM_ (\(l, r) -> check loc l r "loop2") $ zip argsTy valuesTy
    check loc bodyTy ty "loop3"
  where
    bodyTy = getType $ getExprState body
validateType Continue{} = pure ()
