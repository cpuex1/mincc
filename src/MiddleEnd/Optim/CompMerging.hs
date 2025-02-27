{-# LANGUAGE GADTs #-}

module MiddleEnd.Optim.CompMerging (runMergeComp) where

import Control.Monad.State (StateT, evalStateT, gets, modify)
import Data.Map (Map, insert)
import qualified Data.Map as M
import MiddleEnd.Optim.Common (OptimStateT)
import Syntax (BinaryOp (RelationOp), Cond (CComp, CIdentity, CNeg), Expr (..), Ident, KExpr, Pattern (PVar), RelationBinOp, UnaryOp (Not), negateRelation)

data CompContext = CompContext
    { comparison :: Map Ident (RelationBinOp, Ident, Ident)
    , negation :: Map Ident Ident
    }
    deriving (Show, Eq)

type CompStateT m = StateT CompContext (OptimStateT m)

runMergeComp :: (Monad m) => KExpr -> OptimStateT m KExpr
runMergeComp expr =
    evalStateT (mergeComp expr) (CompContext mempty mempty)

mergeComp :: (Monad m) => KExpr -> CompStateT m KExpr
mergeComp (Let state1 (PVar v) (Binary state2 (RelationOp op) lhs rhs) body) = do
    modify $ \ctx ->
        ctx{comparison = insert v (op, lhs, rhs) (comparison ctx)}
    body' <- mergeComp body
    pure $ Let state1 (PVar v) (Binary state2 (RelationOp op) lhs rhs) body'
mergeComp (Let state1 (PVar v) (Unary state2 Not val) body) = do
    compMap <- gets comparison
    case M.lookup val compMap of
        Just (op, lhs, rhs) -> do
            body' <- mergeComp body
            pure $ Let state1 (PVar v) (Binary state2 (RelationOp $ negateRelation op) lhs rhs) body'
        Nothing -> do
            modify $ \ctx ->
                ctx{negation = insert v val (negation ctx)}
            body' <- mergeComp body
            pure $ Let state1 (PVar v) (Unary state2 Not val) body'
mergeComp (If state (CIdentity cond) then' else') = do
    then'' <- mergeComp then'
    else'' <- mergeComp else'

    compMap <- gets comparison
    case M.lookup cond compMap of
        Just (op, lhs, rhs) ->
            pure $ If state (CComp op lhs rhs) then'' else''
        Nothing -> do
            negMap <- gets negation
            case M.lookup cond negMap of
                Just val ->
                    pure $ If state (CNeg val) then'' else''
                Nothing ->
                    pure $ If state (CIdentity cond) then'' else''
mergeComp (If state (CNeg cond) then' else') = do
    then'' <- mergeComp then'
    else'' <- mergeComp else'

    compMap <- gets comparison
    case M.lookup cond compMap of
        Just (op, lhs, rhs) ->
            pure $ If state (CComp (negateRelation op) lhs rhs) then'' else''
        Nothing -> do
            negMap <- gets negation
            case M.lookup cond negMap of
                Just val ->
                    pure $ If state (CIdentity val) then'' else''
                Nothing ->
                    pure $ If state (CNeg cond) then'' else''
mergeComp (Let state1 pattern expr body) = do
    expr' <- mergeComp expr
    body' <- mergeComp body
    pure $ Let state1 pattern expr' body'
mergeComp (If state cond then' else') = do
    then'' <- mergeComp then'
    else'' <- mergeComp else'
    pure $ If state cond then'' else''
mergeComp (Loop state args values body) = do
    body' <- mergeComp body
    pure $ Loop state args values body'
mergeComp expr = pure expr
