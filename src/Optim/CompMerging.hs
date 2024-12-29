module Optim.CompMerging (runMergeComp) where

import Control.Monad.State (StateT, evalStateT, gets, modify)
import Data.Map (Map, empty, insert)
import qualified Data.Map as M
import Optim.Base (OptimStateT)
import Syntax (BinaryOp (RelationOp), Cond (CComp, CIdentity), Expr (..), Ident, KExpr, Pattern (PVar), RelationBinOp)

newtype CompContext = CompContext
    { comparison :: Map Ident (RelationBinOp, Ident, Ident)
    }
    deriving (Show, Eq)

type CompStateT m = StateT CompContext (OptimStateT m)

runMergeComp :: (Monad m) => KExpr -> OptimStateT m KExpr
runMergeComp expr =
    evalStateT (mergeComp expr) (CompContext empty)

mergeComp :: (Monad m) => KExpr -> CompStateT m KExpr
mergeComp (Let state1 (PVar v) (Binary state2 (RelationOp op) lhs rhs) body) = do
    modify $ \ctx ->
        ctx{comparison = insert v (op, lhs, rhs) (comparison ctx)}
    body' <- mergeComp body
    pure $ Let state1 (PVar v) (Binary state2 (RelationOp op) lhs rhs) body'
mergeComp (If state (CIdentity cond) then' else') = do
    then'' <- mergeComp then'
    else'' <- mergeComp else'

    compMap <- gets comparison
    case M.lookup cond compMap of
        Just (op, lhs, rhs) ->
            pure $ If state (CComp op lhs rhs) then'' else''
        Nothing ->
            pure $ If state (CIdentity cond) then'' else''
mergeComp (Let state1 pattern expr body) = do
    expr' <- mergeComp expr
    body' <- mergeComp body
    pure $ Let state1 pattern expr' body'
mergeComp (If state cond then' else') = do
    then'' <- mergeComp then'
    else'' <- mergeComp else'
    pure $ If state cond then'' else''
mergeComp expr = pure expr
