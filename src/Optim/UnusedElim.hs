{-# LANGUAGE GADTs #-}

module Optim.UnusedElim (unusedElim) where

import Control.Monad.State (State, execState, modify)
import Data.Set (Set, delete, empty, insert, member)
import IdentAnalysis (IdentEnvT, removeProp)
import Optim.Pure (definitelyPure)
import Syntax (
    Cond (CComp, CIdentity),
    Expr (..),
    Ident,
    KExpr,
    Pattern (PRec, PTuple, PUnit, PVar),
 )

newtype UnusedElimContext = UnusedElimContext
    { unusedVars :: Set Ident
    }
    deriving (Show, Eq)

type UnusedElimState = State UnusedElimContext

markAsUnused :: Ident -> UnusedElimState ()
markAsUnused ident = do
    modify $ \ctx ->
        ctx{unusedVars = insert ident (unusedVars ctx)}

markAsUsed :: Ident -> UnusedElimState ()
markAsUsed ident = do
    modify $ \ctx ->
        ctx{unusedVars = delete ident (unusedVars ctx)}

searchUnused :: KExpr -> UnusedElimState ()
searchUnused (Const{}) = pure ()
searchUnused (Unary _ _ ident) = do
    markAsUsed ident
searchUnused (Binary _ _ lhs rhs) = do
    markAsUsed lhs
    markAsUsed rhs
searchUnused (If _ (CIdentity c) then' else') = do
    markAsUsed c
    searchUnused then'
    searchUnused else'
searchUnused (If _ (CComp _ lhs rhs) then' else') = do
    markAsUsed lhs
    markAsUsed rhs
    searchUnused then'
    searchUnused else'
searchUnused (Let _ PUnit expr body) = do
    searchUnused expr
    searchUnused body
searchUnused (Let _ (PVar v) expr body) = do
    markAsUnused v
    searchUnused expr
    searchUnused body
searchUnused (Let _ (PRec func _) expr body) = do
    searchUnused expr

    -- Since it can be an unused recursive function,
    -- we need to mark it as used after visiting the function body.
    markAsUnused func
    searchUnused body
searchUnused (Let _ (PTuple _) expr body) = do
    -- Ignore the tuple pattern.
    searchUnused expr
    searchUnused body
searchUnused (Var _ v) = do
    markAsUsed v
searchUnused (App _ f args) = do
    markAsUsed f
    mapM_ markAsUsed args
searchUnused (Tuple _ args) = do
    mapM_ markAsUsed args
searchUnused (ArrayCreate _ size initVal) = do
    markAsUsed size
    markAsUsed initVal
searchUnused (Get _ arr idx) = do
    markAsUsed arr
    markAsUsed idx
searchUnused (Put _ arr idx val) = do
    markAsUsed arr
    markAsUsed idx
    markAsUsed val

unusedElim :: (Monad m) => KExpr -> IdentEnvT m KExpr
unusedElim expr = do
    mapM_ removeProp unused
    pure $ removeUnused expr
  where
    unused = unusedVars $ execState (searchUnused expr) (UnusedElimContext empty)

    removeUnused :: KExpr -> KExpr
    removeUnused (Let state (PVar v) expr' body) =
        if member v unused && definitelyPure expr'
            then
                removeUnused body
            else
                Let state (PVar v) (removeUnused expr') (removeUnused body)
    removeUnused (Let state (PRec func args) expr' body) =
        if member func unused
            then
                -- Since an unused function itself does not have any side effects,
                -- we would not make sure it is pure.
                removeUnused body
            else
                Let state (PRec func args) (removeUnused expr') (removeUnused body)
    removeUnused (Let state pat expr' body) =
        Let state pat (removeUnused expr') (removeUnused body)
    removeUnused (If state cond lhs rhs) =
        If state cond (removeUnused lhs) (removeUnused rhs)
    removeUnused expr' = expr'
