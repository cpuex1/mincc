{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module BackEnd.Optim.UseZeroReg (replaceWithZeroReg) where

import BackEnd.Analysis.Phi (usedInPhi)
import BackEnd.Optim.Common (BackEndOptimStateT)
import CodeBlock (CodeBlock (blockInst), VirtualBlock, VirtualBlockGraph, visitBlock)
import Control.Monad.State (State, modify, runState)
import Data.Maybe (catMaybes)
import Data.Set (Set, insert, member, notMember)
import IR (Inst (IMov), VirtualInst)
import Registers (
    RegID,
    RegMultiple,
    RegOrImm (Imm, Reg),
    RegReplaceable (mapReg),
    RegType (RFloat, RInt),
    Register (Register),
    RegisterKind (SavedReg, ZeroReg),
    updateRT,
    zeroReg,
    (#!!),
 )

type ZeroRegState = State (RegMultiple (Set RegID))

replaceWithZeroReg :: (Monad m) => VirtualBlockGraph -> BackEndOptimStateT m VirtualBlockGraph
replaceWithZeroReg graph =
    pure $
        mapReg
            ( \case
                reg@(Register rTy (SavedReg regID)) ->
                    if regID `member` (zeros #!! rTy) then zeroReg rTy else reg
                reg -> reg
            )
            merged
  where
    banned = usedInPhi graph
    (merged, zeros) = runState (visitBlock (findZeroRegInBlock banned) graph) mempty

findZeroRegInBlock :: RegMultiple (Set RegID) -> VirtualBlock -> ZeroRegState VirtualBlock
findZeroRegInBlock banned block = do
    newInst <- catMaybes <$> mapM (findZeroReg banned) (blockInst block)
    pure $ block{blockInst = newInst}

-- | Find mergeable registers.
findZeroReg :: RegMultiple (Set RegID) -> VirtualInst -> ZeroRegState (Maybe VirtualInst)
findZeroReg banned inst@(IMov _ (Register rTy (SavedReg dest)) (Reg (Register _ ZeroReg))) = do
    if dest `notMember` (banned #!! rTy)
        then do
            modify $ \ctx -> updateRT rTy (insert dest) ctx
            pure Nothing
        else do
            pure $ Just inst
findZeroReg banned (IMov state (Register rTy (SavedReg dest)) (Imm RInt 0)) = do
    if dest `notMember` (banned #!! rTy)
        then do
            modify $ \ctx -> updateRT rTy (insert dest) ctx
            pure Nothing
        else do
            pure $ Just $ IMov state (Register rTy (SavedReg dest)) (Imm rTy 0)
findZeroReg banned (IMov state (Register rTy (SavedReg dest)) (Imm RFloat 0)) = do
    if dest `notMember` (banned #!! rTy)
        then do
            modify $ \ctx -> updateRT rTy (insert dest) ctx
            pure Nothing
        else do
            pure $ Just $ IMov state (Register rTy (SavedReg dest)) (Imm rTy 0)
findZeroReg _ inst = pure $ Just inst
