{-# LANGUAGE GADTs #-}

module MiddleEnd.Optim.UnusedElim (unusedElim) where

import Control.Monad.State (State, execState, modify)
import Control.Monad.Trans (lift)
import Data.Set (Set, delete, empty, insert, member)
import MiddleEnd.Analysis.Common (definitelyPure)
import MiddleEnd.Analysis.Identifier (removeProp)
import MiddleEnd.Optim.Common (OptimStateT)
import Syntax (
    Cond (CComp, CIdentity, CNeg),
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
searchUnused (If _ (CNeg c) then' else') = do
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
searchUnused (Loop _ _ values body) = do
    -- TODO: Implement the loop argument elimination.

    -- The loop arguments should not be marked as used in this time.
    mapM_ markAsUsed values
    searchUnused body
searchUnused (Continue _ args) = do
    mapM_ markAsUsed args

unusedElim :: (Monad m) => KExpr -> OptimStateT m KExpr
unusedElim expr =
    removeUnused expr
  where
    unused = unusedVars $ execState (searchUnused expr) (UnusedElimContext empty)

    removeUnused :: (Monad m) => KExpr -> OptimStateT m KExpr
    removeUnused (Let state (PVar v) expr' body) =
        if member v unused && definitelyPure expr'
            then do
                lift $ removeProp v
                removeUnused body
            else do
                removedExpr <- removeUnused expr'
                removedBody <- removeUnused body
                pure $ Let state (PVar v) removedExpr removedBody
    removeUnused (Let state (PRec func args) expr' body) =
        if member func unused
            then do
                -- Since an unused function itself does not have any side effects,
                -- we would not make sure it is pure.
                lift $ removeProp func
                removeUnused body
            else do
                removedExpr <- removeUnused expr'
                removedBody <- removeUnused body
                pure $ Let state (PRec func args) removedExpr removedBody
    removeUnused (Let state pat expr' body) = do
        removedExpr <- removeUnused expr'
        removedBody <- removeUnused body
        pure $ Let state pat removedExpr removedBody
    removeUnused (If state cond lhs rhs) = do
        removedLeft <- removeUnused lhs
        removedRight <- removeUnused rhs
        pure $ If state cond removedLeft removedRight
    removeUnused (Loop state args values body) = do
        removedBody <- removeUnused body
        pure $ Loop state args values removedBody
    removeUnused expr' = pure expr'
