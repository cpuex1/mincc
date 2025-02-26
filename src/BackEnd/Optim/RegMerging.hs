module BackEnd.Optim.RegMerging (regMerging) where

import BackEnd.Analysis.Phi (usedInPhi)
import BackEnd.Optim.Common (BackEndOptimStateT)
import CodeBlock (CodeBlock (blockInst), VirtualBlock, VirtualBlockGraph, visitBlock)
import Control.Monad.State (State, modify, runState)
import Data.Map (singleton)
import Data.Maybe (catMaybes)
import Data.Set (Set, notMember)
import IR (Inst (IMov), VirtualInst)
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
    (merged, mapping) = runState (visitBlock (mergeRegBlock banned) graph) mempty

mergeRegBlock :: RegMultiple (Set RegID) -> VirtualBlock -> RegMergingState VirtualBlock
mergeRegBlock banned block = do
    newInst <- catMaybes <$> mapM (findMergeableReg banned) (blockInst block)
    pure $ block{blockInst = newInst}

-- | Find mergeable registers.
findMergeableReg :: RegMultiple (Set RegID) -> VirtualInst -> RegMergingState (Maybe VirtualInst)
findMergeableReg banned inst@(IMov _ (Register rTy (SavedReg dest)) (Reg (Register _ (SavedReg src)))) = do
    if dest `notMember` (banned #!! rTy)
        then do
            modify $ \ctx -> updateRT rTy (\mapping -> mapping <> RegMapping (singleton dest src)) ctx
            pure Nothing
        else do
            pure $ Just inst
findMergeableReg _ inst = pure $ Just inst
