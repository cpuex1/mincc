{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module BackEnd.Optim.Common (
    mapPhi,
    insertPhi,
    BackEndOptimContext (..),
    BackEndOptimStateT,
) where

import BackEnd.BackendEnv (BackendStateT)
import Control.Monad.State (MonadState (put), StateT, runState)
import Data.Map (Map, insert, singleton)
import Data.Maybe (catMaybes)
import IR (Inst (IPhi), InstKind (AllowPhi, InstStateTy), InstLabel)
import Registers (RegType (RFloat, RInt), Register (Register))

-- | Search for a phi instruction and apply a function to it if found.
mapPhi ::
    (Monad m) =>
    Register a ->
    (InstStateTy ty -> Map InstLabel (Register a) -> m (Maybe (Inst ty))) ->
    Inst ty ->
    m (Maybe (Inst ty))
mapPhi target@(Register RInt _) func inst@(IPhi state dest@(Register RInt _) srcs) =
    if dest == target
        then
            func state srcs
        else pure (Just inst)
mapPhi target@(Register RFloat _) func inst@(IPhi state dest@(Register RFloat _) srcs) =
    if dest == target
        then
            func state srcs
        else pure (Just inst)
mapPhi _ _ inst = pure (Just inst)

-- | Insert a phi instruction.
insertPhi :: (AllowPhi ty ~ True) => InstStateTy ty -> Register a -> InstLabel -> Register a -> [Inst ty] -> [Inst ty]
insertPhi newState dest fromLabel src block =
    if visited
        then catMaybes inst
        else newPhi : block
  where
    -- Visit the phi node and modify it.
    -- If the phi node does not exist, return the original block.
    (inst, visited) =
        runState
            ( mapM
                ( mapPhi
                    dest
                    ( \state srcs -> do
                        put True
                        pure $ Just (IPhi state dest (insert fromLabel src srcs))
                    )
                )
                block
            )
            False

    -- Create a new phi instruction if it does not exist.
    newPhi = IPhi newState dest $ singleton fromLabel src

data BackEndOptimContext = BackEndOptimContext
    deriving (Show, Eq)

type BackEndOptimStateT m = StateT BackEndOptimContext (BackendStateT m)
