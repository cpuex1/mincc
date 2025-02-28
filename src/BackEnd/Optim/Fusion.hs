{-# LANGUAGE OverloadedStrings #-}

module BackEnd.Optim.Fusion (runFusion) where

import BackEnd.Analysis.Liveness (runGraphLiveness)
import BackEnd.Liveness (Liveness (alive), LivenessBlock, LivenessInst, LivenessLoc (LivenessLoc, livenessLoc))
import BackEnd.Optim.Common (BackEndOptimStateT)
import CodeBlock (
    BlockGraph (graphBlocks),
    CodeBlock (CodeBlock, blockInst, blockName, prevBlocks, terminator),
    VirtualBlock,
    VirtualBlockGraph,
 )
import Data.Set (notMember)
import IR (Inst (IFOp, IRawInst), VirtualInst, substIState)
import Registers (RegTuple (createRT), RegType (RFloat), Register (Register), RegisterKind (SavedReg), (#!!))
import Syntax (FloatBinOp (FAdd, FMul))

runFusion :: (Monad m) => VirtualBlockGraph -> BackEndOptimStateT m VirtualBlockGraph
runFusion graph = do
    let livenessGraph = runGraphLiveness graph
    pure graph{graphBlocks = map fusionBlock $ graphBlocks livenessGraph}

fusionBlock :: LivenessBlock -> VirtualBlock
fusionBlock block =
    CodeBlock (blockName block) (fusion $ blockInst block) (prevBlocks block) (terminator block)

fusion :: [LivenessInst] -> [VirtualInst]
fusion [] = []
fusion (inst1@(IFOp loc1 FMul dest@(Register _ (SavedReg destID)) src1 src2) : (inst2@(IFOp (LivenessLoc _ live) FAdd dest' src1' src2') : rest)) =
    if dest == src1' && dest /= src2' && destID `notMember` alive (live #!! RFloat)
        then
            IRawInst (livenessLoc loc1) "fmadd" dest' (createRT [] [src1, src2, src2']) : fusion rest
        else
            substIState livenessLoc inst1
                : fusion (inst2 : rest)
fusion (inst : rest) =
    substIState livenessLoc inst : fusion rest
