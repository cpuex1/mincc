{-# LANGUAGE GADTs #-}

module BackEnd.Optim.ArgsRegRev (replaceSavedRegRevInGraph) where

import BackEnd.Analysis.IR (inOutRegisters', inSet', outSet')
import BackEnd.Analysis.Liveness (runGraphLiveness)
import BackEnd.Liveness (
    Liveness (alive),
    LivenessBlock,
    LivenessInst,
    LivenessLoc (LivenessLoc, livenessLoc),
 )
import BackEnd.Optim.Common (BackEndOptimStateT)
import CodeBlock (BlockGraph (graphBlocks), VirtualBlock, VirtualBlockGraph, visitInst)
import Control.Monad (when)
import Control.Monad.State (State, gets, modify, runState)
import Data.Map (Map, lookup)
import qualified Data.Map as M
import Data.Set (Set, insert, notMember)
import IR (Inst (..), VirtualInst, substIState)
import Registers (
    RegID,
    RegOrImm (Reg),
    RegType (RFloat, RInt),
    RegVariant,
    Register (Register),
    RegisterKind (ArgsReg, SavedReg),
    buildRT,
    eachRegType,
    updateRT,
    weakMapReg,
    (#!!),
    (#$),
 )
import Prelude hiding (lookup)

data ArgsRegRevContext rTy = ArgsRegRevContext
    { savedRegInfo :: Map RegID (Set RegID)
    , regMapping :: Register rTy -> Register rTy
    }

type ArgsRegRevState = State (RegVariant ArgsRegRevContext)

addSavedReg :: Register a -> ArgsRegRevState ()
addSavedReg (Register rTy (SavedReg regID)) =
    modify (updateRT rTy (\ctx -> ctx{savedRegInfo = M.insert regID mempty $ savedRegInfo ctx}))
addSavedReg _ = pure ()

ban :: Register a -> ArgsRegRevState ()
ban (Register rTy (ArgsReg arg)) =
    modify (updateRT rTy (\ctx -> ctx{savedRegInfo = M.map (insert arg) $ savedRegInfo ctx}))
ban _ = pure ()

addRegMapping :: RegType a -> (Register a -> Register a) -> ArgsRegRevState ()
addRegMapping rTy f =
    modify (updateRT rTy (\ctx -> ctx{regMapping = f . regMapping ctx}))

replaceSavedRegRevInGraph :: (Monad m) => VirtualBlockGraph -> BackEndOptimStateT m VirtualBlockGraph
replaceSavedRegRevInGraph graph = do
    let livenessGraph = runGraphLiveness graph
    pure $ graph{graphBlocks = map replaceArgs (graphBlocks livenessGraph)}

replaceArgs :: LivenessBlock -> VirtualBlock
replaceArgs block =
    weakMapReg RFloat fMap $ weakMapReg RInt iMap block'
  where
    (block', state) = runState (visitInst findReplaceable block) $ buildRT (const (ArgsRegRevContext mempty id))
    iMap = regMapping (state #!! RInt)
    fMap = regMapping (state #!! RFloat)

findReplaceable :: LivenessInst -> ArgsRegRevState VirtualInst
findReplaceable inst@(IMov (LivenessLoc _ live) dest@(Register rTy (ArgsReg arg)) (Reg src@(Register _ (SavedReg regID)))) = do
    ctx <- gets (savedRegInfo . (#!! rTy))
    case lookup regID ctx of
        Just banned ->
            when (arg `notMember` banned && regID `notMember` alive (live #!! rTy)) $ do
                addRegMapping rTy $ \reg ->
                    if reg == src then dest else reg
        Nothing -> pure ()
    ban dest
    pure $ substIState livenessLoc inst
findReplaceable inst@IPhi{} =
    pure $ substIState livenessLoc inst
findReplaceable inst@ICall{} = do
    modify (const (\ctx -> ctx{savedRegInfo = mempty}) #$)
    pure $ substIState livenessLoc inst
findReplaceable inst@ICallReg{} = do
    modify (const (\ctx -> ctx{savedRegInfo = mempty}) #$)
    pure $ substIState livenessLoc inst
findReplaceable inst = do
    let inOut = inOutRegisters' inst
    eachRegType $
        \rTy -> do
            mapM_ ban (inSet' $ inOut #!! rTy)
            mapM_ ban (outSet' $ inOut #!! rTy)
            mapM_ addSavedReg (outSet' $ inOut #!! rTy)
    pure $ substIState livenessLoc inst
