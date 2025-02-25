{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module MiddleEnd.Optim.ReadOnly (removeReadOnlyArrays) where

import Control.Monad (when)
import Control.Monad.State (StateT, execStateT, gets, modify)
import Control.Monad.Trans (lift)
import Data.Map (Map, delete, lookup)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Set (Set, insert, singleton, toList)
import FrontEnd.Flatten (flattenExpr)
import MiddleEnd.Analysis.Identifier (asConstant, genNewVar, getTyOf)
import MiddleEnd.Optim.Common (OptimStateT)
import Syntax (
    Expr (..),
    Ident,
    KExpr,
    Literal (LInt, LUnit),
    Pattern (PRec, PTuple, PVar),
    TypedState (TypedState),
    dummyLoc,
 )
import Typing (TypeKind (TArray))
import Prelude hiding (lookup)

data ArrayContext = ArrayContext
    { multipleTime :: Bool
    , banned :: Set Ident
    , participants :: Map Ident (Set Int)
    }
    deriving (Show, Eq)

type ArrayStateT m = StateT ArrayContext (OptimStateT m)

-- | Marks the given identifier as not read-only.
ban :: (Monad m) => Ident -> ArrayStateT m ()
ban arr = do
    ty <- lift $ lift $ getTyOf arr
    case ty of
        TArray _ ->
            modify $ \ctx -> ctx{banned = insert arr $ banned ctx, participants = delete arr $ participants ctx}
        _ -> pure ()

-- | Gets whether the given identifier is marked as banned.
isBanned :: (Monad m) => Ident -> ArrayStateT m Bool
isBanned arr = do
    bannedSet <- gets banned
    pure $ arr `elem` bannedSet

-- | Adds the writing history of the given array.
addAccessPattern :: (Monad m) => Bool -> Ident -> Ident -> ArrayStateT m ()
addAccessPattern isWriting arr idx = do
    ty <- lift $ lift $ getTyOf arr
    banned' <- isBanned arr
    case (ty, banned') of
        (TArray _, False) -> do
            -- The array has not been banned.
            multipleTime' <- gets multipleTime
            if multipleTime' && isWriting
                then
                    -- The array can be written multiple times.
                    ban arr
                else do
                    c <- lift $ lift $ asConstant idx
                    case c of
                        Just (LInt idx') -> do
                            when isWriting $
                                do
                                    accessPattern <- gets (lookup arr . participants)
                                    case accessPattern of
                                        Just accessPattern' ->
                                            if idx' `elem` accessPattern'
                                                then
                                                    -- The array is written with the same index.
                                                    ban arr
                                                else modify $ \ctx ->
                                                    ctx{participants = M.insert arr (insert idx' accessPattern') $ participants ctx}
                                        Nothing ->
                                            -- Add a new writing pattern.
                                            modify $ \ctx ->
                                                ctx{participants = M.insert arr (singleton idx') $ participants ctx}
                        _ ->
                            -- The array is accessed with a non-constant index.
                            ban arr
        _ -> pure ()

-- | Visits the given expression to search for the writing pattern.
searchWritingPattern :: (Monad m) => KExpr -> ArrayStateT m ()
searchWritingPattern (Put _ arr idx val) = do
    ban val
    addAccessPattern True arr idx
searchWritingPattern (Get _ arr idx) = addAccessPattern False arr idx
searchWritingPattern (If _ _ thenExpr elseExpr) = do
    searchWritingPattern thenExpr
    searchWritingPattern elseExpr
searchWritingPattern (Let _ (PRec _ _) expr body) = do
    prevMultipleTime <- gets multipleTime

    -- The function can be called multiple times.
    modify $ \ctx -> ctx{multipleTime = True}
    searchWritingPattern expr
    modify $ \ctx -> ctx{multipleTime = prevMultipleTime}

    searchWritingPattern body
searchWritingPattern (Let _ (PVar _) (ArrayCreate{}) body) = do
    searchWritingPattern body
searchWritingPattern (Let _ (PVar v) expr body) = do
    -- The array can be captured by other variables.
    ban v
    searchWritingPattern expr
    searchWritingPattern body
searchWritingPattern (Let _ (PTuple values) expr body) = do
    -- The array can be captured by other variables.
    mapM_ ban values
    searchWritingPattern expr
    searchWritingPattern body
searchWritingPattern (Loop _ _ _ body) = do
    prevMultipleTime <- gets multipleTime

    -- The loop body can be executed multiple times.
    modify $ \ctx -> ctx{multipleTime = True}
    searchWritingPattern body
    modify $ \ctx -> ctx{multipleTime = prevMultipleTime}
searchWritingPattern (App _ _ args) =
    -- The array can be modified by the function.
    mapM_ ban args
searchWritingPattern (Var _ v) = do
    -- The array can be captured by other variables.
    ban v
searchWritingPattern (Tuple _ values) =
    -- The array can be captured by other tuples.
    mapM_ ban values
searchWritingPattern _ = pure ()

-- | Gets the read-only arrays in the given expression.
readOnlyArrays :: (Monad m) => KExpr -> OptimStateT m (Map Ident (Set Int))
readOnlyArrays expr = do
    evaluated <- execStateT (searchWritingPattern expr) $ ArrayContext False mempty mempty
    pure $ participants evaluated

-- | Replaces the read-only arrays with the corresponding variables.
replaceReadOnly :: (Monad m) => Map (Ident, Int) Ident -> KExpr -> OptimStateT m KExpr
replaceReadOnly mapping inst@(Get state arr idx) = do
    c <- lift $ asConstant idx
    case c of
        Just (LInt idx') -> do
            case lookup (arr, idx') mapping of
                Just var -> pure $ Var state var
                Nothing -> pure inst
        _ -> pure inst
replaceReadOnly mapping inst@(Put state arr idx val) = do
    c <- lift $ asConstant idx
    case c of
        Just (LInt idx') -> do
            case lookup (arr, idx') mapping of
                Just var -> do
                    vTy <- lift $ getTyOf val
                    pure $ Let state (PVar var) (Var (TypedState vTy dummyLoc) val) (Const state LUnit)
                Nothing -> pure inst
        _ -> pure inst
replaceReadOnly mapping (If state cond thenExpr elseExpr) = do
    thenExpr' <- replaceReadOnly mapping thenExpr
    elseExpr' <- replaceReadOnly mapping elseExpr
    pure $ If state cond thenExpr' elseExpr'
replaceReadOnly mapping (Let state pat expr body) = do
    expr' <- replaceReadOnly mapping expr
    body' <- replaceReadOnly mapping body
    pure $ Let state pat expr' body'
replaceReadOnly mapping (Loop state args values body) = do
    body' <- replaceReadOnly mapping body
    pure $ Loop state args values body'
replaceReadOnly _ expr = pure expr

-- | Removes the read-only arrays in the given expression.
removeReadOnlyArrays :: (Monad m) => KExpr -> OptimStateT m KExpr
removeReadOnlyArrays expr = do
    readOnly <- readOnlyArrays expr
    let accessPattern = concatMap (\(ident, access) -> map (ident,) $ toList access) $ M.toList readOnly
    mapping <-
        M.fromList . catMaybes
            <$> mapM
                ( \(ident, access) -> do
                    -- Generates a new variable for each element of the array.
                    ty <- lift $ getTyOf ident
                    case ty of
                        TArray elemTy -> do
                            element <- lift $ genNewVar elemTy
                            pure $ Just ((ident, access), element)
                        _ -> pure Nothing
                )
                accessPattern
    replaced <- replaceReadOnly mapping expr
    pure $ flattenExpr replaced
