{-# LANGUAGE GADTs #-}

module BackEnd.Optim.RegMerging (regMerging) where

import BackEnd.Analysis.IR (InOutSet (outSet), inOutRegisters)
import BackEnd.Analysis.Liveness (runGraphLiveness)
import BackEnd.Analysis.Phi (usedInPhi)
import BackEnd.Liveness (Liveness (alive), LivenessBlock, LivenessInst, LivenessLoc (livenessLoc, livenessProp))
import BackEnd.Optim.Common (BackEndOptimStateT)
import CodeBlock (CodeBlock (blockInst), VirtualBlock, VirtualBlockGraph, visitBlock)
import Control.Monad.State (State, modify, runState)
import Data.Map (singleton)
import Data.Maybe (catMaybes)
import Data.Set (Set, member, notMember)
import IR (Inst (IMov, IPhi), VirtualInst, substIState)
import Registers (
    RegID,
    RegMapping (RegMapping),
    RegMultiple,
    RegOrImm (Reg),
    Register (Register),
    RegisterKind (SavedReg),
    applyRegMapping,
    updateRT,
    (#!!),
 )

type RegMergingState = State (RegMultiple RegMapping)

regMerging :: (Monad m) => VirtualBlockGraph -> BackEndOptimStateT m VirtualBlockGraph
regMerging graph =
    pure $ applyRegMapping mapping merged
  where
    banned = usedInPhi graph
    livenessGraph = runGraphLiveness graph
    (merged, mapping) = runState (visitBlock (mergeRegBlock banned) livenessGraph) mempty

mergeRegBlock :: RegMultiple (Set RegID) -> LivenessBlock -> RegMergingState VirtualBlock
mergeRegBlock banned block = do
    let defined = definedInBlock block
    newInst <- catMaybes <$> mapM (findMergeableReg banned defined) (blockInst block)
    pure $ block{blockInst = newInst}

-- | Get the registers defined in a block. This function ignores phi instructions.
definedInBlock :: CodeBlock a -> RegMultiple (Set RegID)
definedInBlock block =
    foldl
        ( \defined i ->
            case i of
                IPhi{} -> defined
                _ ->
                    let defined' = outSet <$> inOutRegisters i
                     in defined' <> defined
        )
        mempty
        (blockInst block)

-- | Find mergeable registers.
findMergeableReg :: RegMultiple (Set RegID) -> RegMultiple (Set RegID) -> LivenessInst -> RegMergingState (Maybe VirtualInst)
findMergeableReg banned defined inst@(IMov state (Register rTy (SavedReg dest)) (Reg (Register _ (SavedReg src)))) = do
    if dest `notMember` (banned #!! rTy)
        then do
            -- The destination register is not used in a phi instruction.
            modify $ \ctx -> updateRT rTy (\mapping -> mapping <> RegMapping (singleton dest src)) ctx
            pure Nothing
        else do
            let live = alive $ livenessProp state #!! rTy
            if src `member` (defined #!! rTy) && src `notMember` live
                then do
                    -- The destination register is used in a phi instruction,
                    -- but it can be replaced with the source register.
                    modify $ \ctx -> updateRT rTy (\mapping -> mapping <> RegMapping (singleton dest src)) ctx
                    pure Nothing
                else
                    pure $ Just $ substIState livenessLoc inst
findMergeableReg _ _ inst@(IMov _ reg1 (Reg reg2)) =
    if reg1 == reg2
        then
            pure Nothing
        else
            pure $ Just $ substIState livenessLoc inst
findMergeableReg _ _ inst = pure $ Just $ substIState livenessLoc inst
