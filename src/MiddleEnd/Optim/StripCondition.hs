{-# LANGUAGE GADTs #-}

module MiddleEnd.Optim.StripCondition (
    stripConditionM,
) where

import FrontEnd.Flatten (flattenExpr)
import MiddleEnd.Optim.Common (OptimStateT)
import Syntax (
    BinaryOp (RelationOp),
    Cond (CComp, CIdentity, CNeg),
    Expr (..),
    KExpr,
    Literal (LInt, LUnit),
    Pattern (PUnit),
    TState (TState),
    UnaryOp (Not),
    dummyLoc,
    negateRelation,
 )
import Typing (TypeBase (TUnit))

lastExpr :: KExpr -> KExpr
lastExpr (Let _ _ _ body) = lastExpr body
lastExpr expr = expr

replaceLastWithUnit :: KExpr -> KExpr
replaceLastWithUnit (Let (TState _ loc) pat expr body) = Let (TState TUnit loc) pat expr (replaceLastWithUnit body)
replaceLastWithUnit _ = Const (TState TUnit dummyLoc) LUnit

stripConditionM :: (Monad m) => KExpr -> OptimStateT m KExpr
stripConditionM = pure . flattenExpr . stripCondition

stripCondition :: KExpr -> KExpr
stripCondition (If (TState ty loc) cond thenBody elseBody) =
    case (cond, lastExpr thenBody', lastExpr elseBody') of
        (CIdentity cond', Const _ (LInt 1), Const _ (LInt 0)) ->
            Let (TState ty loc) PUnit replacedIf (Var (TState ty loc) cond')
        (CIdentity cond', Const _ (LInt 0), Const _ (LInt 1)) ->
            Let (TState ty loc) PUnit replacedIf (Unary (TState ty loc) Not cond')
        (CNeg cond', Const _ (LInt 0), Const _ (LInt 1)) ->
            Let (TState ty loc) PUnit replacedIf (Var (TState ty loc) cond')
        (CNeg cond', Const _ (LInt 1), Const _ (LInt 0)) ->
            Let (TState ty loc) PUnit replacedIf (Unary (TState ty loc) Not cond')
        (CComp op lhs rhs, Const _ (LInt 1), Const _ (LInt 0)) ->
            Let (TState ty loc) PUnit replacedIf (Binary (TState ty loc) (RelationOp op) lhs rhs)
        (CComp op lhs rhs, Const _ (LInt 0), Const _ (LInt 1)) ->
            Let (TState ty loc) PUnit replacedIf (Binary (TState ty loc) (RelationOp (negateRelation op)) lhs rhs)
        _ ->
            If (TState ty loc) cond thenBody' elseBody'
  where
    thenBody' = stripCondition thenBody
    elseBody' = stripCondition elseBody
    replacedThen = replaceLastWithUnit thenBody'
    replacedElse = replaceLastWithUnit elseBody'
    replacedIf = If (TState TUnit loc) cond replacedThen replacedElse
stripCondition (Let state pat expr body) =
    Let state pat (stripCondition expr) (stripCondition body)
stripCondition (Loop state args values body) =
    Loop state args values (stripCondition body)
stripCondition expr = expr
