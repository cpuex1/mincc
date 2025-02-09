{-# LANGUAGE GADTs #-}

module FrontEnd.Flatten (flattenExpr) where

import Syntax

flattenExpr :: KExpr -> KExpr
flattenExpr (If s cond thenExpr elseExpr) =
    If s cond (flattenExpr thenExpr) (flattenExpr elseExpr)
flattenExpr (Let s (PRec f args) value body) =
    Let s (PRec f args) (flattenExpr value) (flattenExpr body)
flattenExpr (Let (TypedState ty loc) pat value body) =
    insert (flattenExpr value)
  where
    insert :: KExpr -> KExpr
    insert (Let (TypedState _ loc') pat' value' body') =
        Let (TypedState ty loc') pat' value' (insert body')
    insert expr = Let (TypedState ty loc) pat expr (flattenExpr body)
flattenExpr (Loop s args values body) =
    Loop s args values (flattenExpr body)
flattenExpr e = e
