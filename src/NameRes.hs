{-# LANGUAGE GADTs #-}

module NameRes (resolveNames) where

import Data.Map (Map, empty, insert, lookup)
import Data.Text (Text)
import Syntax
import Prelude hiding (lookup)

type VarTable = Map Text Loc

resolveNames :: ParsedExpr -> ResolvedExpr
resolveNames = resolveNames' empty
  where
    -- Do not use State monad here, because its side effects can break this process.
    resolveNames' :: VarTable -> ParsedExpr -> ResolvedExpr
    resolveNames' _ (PGuard (Const pos lit)) = RGuard (Const pos lit)
    resolveNames' table (PGuard (Unary pos op expr)) = RGuard (Unary pos op (resolveNames' table expr))
    resolveNames' table (PGuard (Binary pos op expr1 expr2)) =
        RGuard (Binary pos op (resolveNames' table expr1) (resolveNames' table expr2))
    resolveNames' table (PGuard (If pos (CIdentity cond) thenExpr elseExpr)) =
        RGuard (If pos (CIdentity (resolveNames' table cond)) thenExpr' elseExpr')
      where
        thenExpr' = rExp (resolveNames' table $ PGuard thenExpr)
        elseExpr' = rExp (resolveNames' table $ PGuard elseExpr)
    resolveNames' table (PGuard (Let pos PUnit expr body)) =
        RGuard (Let pos PUnit expr' body')
      where
        expr' = rExp (resolveNames' table $ PGuard expr)
        body' = rExp (resolveNames' table $ PGuard body)
    resolveNames' table (PGuard (Let pos (PVar (RawIdent identPos ident)) expr body)) =
        RGuard (Let pos (PVar (UserDefined identPos ident)) expr' body')
      where
        expr' = rExp (resolveNames' table $ PGuard expr)
        body' = rExp (resolveNames' (insert ident identPos table) $ PGuard body)
    resolveNames' table (PGuard (Let pos (PRec (RawIdent funcPos func) args) expr body)) =
        RGuard (Let pos (PRec (UserDefined funcPos func) args') expr' body')
      where
        args' = map (\(RawIdent argPos arg) -> UserDefined argPos arg) args

        tableWithFunc = insert func funcPos table
        tableWithArgs = foldl (\t (RawIdent argPos arg) -> insert arg argPos t) tableWithFunc args

        expr' = rExp (resolveNames' tableWithArgs $ PGuard expr)
        body' = rExp (resolveNames' tableWithFunc $ PGuard body)
    resolveNames' table (PGuard (Let pos (PTuple values) expr body)) =
        RGuard (Let pos (PTuple values') expr' body')
      where
        valuesTable = foldl (\t (RawIdent pos' value) -> insert value pos' t) table values
        values' = map (\(RawIdent pos' value) -> UserDefined pos' value) values
        expr' = rExp (resolveNames' table $ PGuard expr)
        body' = rExp (resolveNames' valuesTable $ PGuard body)
    resolveNames' table (PGuard (Var pos (RawIdent _ ident))) =
        case lookup ident table of
            Just pos' -> RGuard (Var pos (UserDefined pos' ident))
            Nothing -> RGuard (Var pos (ExternalIdent ident))
    resolveNames' table (PGuard (App pos func args)) =
        RGuard (App pos func' args')
      where
        func' = resolveNames' table func
        args' = map (resolveNames' table) args
    resolveNames' table (PGuard (Tuple pos values)) =
        RGuard (Tuple pos values')
      where
        values' = map (resolveNames' table) values
    resolveNames' table (PGuard (ArrayCreate pos size value)) =
        RGuard (ArrayCreate pos size' value')
      where
        size' = resolveNames' table size
        value' = resolveNames' table value
    resolveNames' table (PGuard (Get pos array index)) =
        RGuard (Get pos array' index')
      where
        array' = resolveNames' table array
        index' = resolveNames' table index
    resolveNames' table (PGuard (Put pos array index value)) =
        RGuard (Put pos array' index' value')
      where
        array' = resolveNames' table array
        index' = resolveNames' table index
        value' = resolveNames' table value
