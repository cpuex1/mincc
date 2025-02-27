{-# LANGUAGE GADTs #-}

module FrontEnd.Flatten (flattenExpr) where

import Syntax (
    Expr (If, Let, Loop),
    KExpr,
    Pattern (PRec),
    TState (TState),
 )

flattenExpr :: KExpr -> KExpr
flattenExpr (If s cond thenExpr elseExpr) =
    If s cond (flattenExpr thenExpr) (flattenExpr elseExpr)
flattenExpr (Let s (PRec f args) value body) =
    Let s (PRec f args) (flattenExpr value) (flattenExpr body)
flattenExpr (Let (TState ty loc) pat value body) =
    insert (flattenExpr value)
  where
    insert :: KExpr -> KExpr
    insert (Let (TState _ loc') pat' value' body') =
        Let (TState ty loc') pat' value' (insert body')
    insert expr = Let (TState ty loc) pat expr (flattenExpr body)
flattenExpr (Loop s args values body) =
    Loop s args values (flattenExpr body)
flattenExpr e = e
