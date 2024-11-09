{-# LANGUAGE GADTs #-}

module Flatten (flattenExpr) where

import Syntax

flattenExpr :: KExpr -> KExpr
flattenExpr (If s cond thenExpr elseExpr) =
    If s cond (flattenExpr thenExpr) (flattenExpr elseExpr)
flattenExpr (Let s (PRec f args) value body) =
    Let s (PRec f args) (flattenExpr value) (flattenExpr body)
flattenExpr (Let s pat value body) =
    insert (flattenExpr value)
  where
    insert :: KExpr -> KExpr
    insert (Let s' pat' value' body') =
        Let s' pat' value' (insert body')
    insert expr = Let s pat expr (flattenExpr body)
flattenExpr e = e
