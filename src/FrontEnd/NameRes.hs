{-# LANGUAGE GADTs #-}

module FrontEnd.NameRes (resolveNames) where

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
    resolveNames' _ (Const pos lit) = Const pos lit
    resolveNames' table ((Unary pos op expr)) = Unary pos op (resolveNames' table expr)
    resolveNames' table ((Binary pos op expr1 expr2)) =
        Binary pos op (resolveNames' table expr1) (resolveNames' table expr2)
    resolveNames' table ((If pos (CIdentity cond) thenExpr elseExpr)) =
        If pos (CIdentity (resolveNames' table cond)) thenExpr' elseExpr'
      where
        thenExpr' = resolveNames' table $ thenExpr
        elseExpr' = resolveNames' table $ elseExpr
    resolveNames' table ((Let pos PUnit expr body)) =
        Let pos PUnit expr' body'
      where
        expr' = resolveNames' table $ expr
        body' = resolveNames' table $ body
    resolveNames' table ((Let pos (PVar (RawIdent identPos ident)) expr body)) =
        Let pos (PVar (UserDefined identPos ident)) expr' body'
      where
        expr' = resolveNames' table expr
        body' = resolveNames' (insert ident identPos table) body
    resolveNames' table ((Let pos (PRec (RawIdent funcPos func) args) expr body)) =
        Let pos (PRec (UserDefined funcPos func) args') expr' body'
      where
        args' = map (\(RawIdent argPos arg) -> UserDefined argPos arg) args

        tableWithFunc = insert func funcPos table
        tableWithArgs = foldl (\t (RawIdent argPos arg) -> insert arg argPos t) tableWithFunc args

        expr' = resolveNames' tableWithArgs expr
        body' = resolveNames' tableWithFunc body
    resolveNames' table ((Let pos (PTuple values) expr body)) =
        Let pos (PTuple values') expr' body'
      where
        valuesTable = foldl (\t (RawIdent pos' value) -> insert value pos' t) table values
        values' = map (\(RawIdent pos' value) -> UserDefined pos' value) values
        expr' = resolveNames' table expr
        body' = resolveNames' valuesTable body
    resolveNames' table ((Var pos (RawIdent _ ident))) =
        case lookup ident table of
            Just pos' -> Var pos (UserDefined pos' ident)
            Nothing -> Var pos (ExternalIdent ident)
    resolveNames' table ((App pos func args)) =
        App pos func' args'
      where
        func' = resolveNames' table func
        args' = map (resolveNames' table) args
    resolveNames' table ((Tuple pos values)) =
        Tuple pos values'
      where
        values' = map (resolveNames' table) values
    resolveNames' table ((ArrayCreate pos size value)) =
        ArrayCreate pos size' value'
      where
        size' = resolveNames' table size
        value' = resolveNames' table value
    resolveNames' table ((Get pos array index)) =
        Get pos array' index'
      where
        array' = resolveNames' table array
        index' = resolveNames' table index
    resolveNames' table ((Put pos array index value)) =
        Put pos array' index' value'
      where
        array' = resolveNames' table array
        index' = resolveNames' table index
        value' = resolveNames' table value
