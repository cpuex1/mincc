module BackEnd.Optim.UnusedReg (removeUnusedReg) where

import BackEnd.Analysis.IR (InOutSet (outSet), inOutRegisters)
import BackEnd.Analysis.Liveness (runGraphLiveness)
import BackEnd.Liveness (Liveness (alive), LivenessInst, LivenessLoc (livenessLoc, livenessProp))
import BackEnd.Optim.Common (BackEndOptimStateT)
import CodeBlock (CodeBlock (..), VirtualBlockGraph, visitBlock)
import Data.Set (Set, intersection, null)
import IR (Inst (IRawInst), getIState, substIState)
import Registers (RegID, RegMultiple, RegType (RFloat, RInt), buildRT, (#!!))
import Prelude hiding (null)

removeUnusedReg :: (Monad m) => VirtualBlockGraph -> BackEndOptimStateT m VirtualBlockGraph
removeUnusedReg graph = do
    let livenessGraph = runGraphLiveness graph
    visitBlock
        ( \block -> do
            let inst' = map (substIState livenessLoc) $ filter isMeaningful $ blockInst block
            pure $ CodeBlock (blockName block) inst' (prevBlocks block) (terminator block)
        )
        livenessGraph

isMeaningful :: LivenessInst -> Bool
isMeaningful IRawInst{} = True
isMeaningful inst =
    hasNoOutput || not outputNotUsed
  where
    outSet' = outSet <$> inOutRegisters inst
    alive' = alive <$> livenessProp (getIState inst)
    definedAndUsed = buildRT (\rTy -> (outSet' #!! rTy) `intersection` (alive' #!! rTy)) :: RegMultiple (Set RegID)
    hasNoOutput = null (outSet' #!! RInt) && null (outSet' #!! RFloat)
    outputNotUsed = null (definedAndUsed #!! RInt) && null (definedAndUsed #!! RFloat)
