{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module BackEnd.Refuge (runRefugeBlock) where

import BackEnd.Analysis.IR (InOutSet (InOutSet), inOutRegisters)
import BackEnd.Liveness (
    Liveness (Liveness, alive),
    LivenessBlock,
    LivenessGraph,
    LivenessInst,
    LivenessLoc (LivenessLoc, livenessLoc, livenessProp),
 )
import CodeBlock (BlockGraph (..), CodeBlock (..), VirtualBlock, VirtualBlockGraph)
import Control.Monad.State (State, gets, modify, runState)
import Data.Foldable (toList)
import Data.List (unsnoc)
import Data.Map (Map, insert, keys, lookup, notMember)
import Data.Maybe (catMaybes, maybeToList)
import Data.Set (Set, fromList, intersection, member)
import qualified Data.Set as S
import Data.Text (isPrefixOf)
import IR (
    Inst (..),
    VirtualInst,
    getIState,
    substIState,
 )
import Registers (
    RegID,
    RegMultiple,
    RegType (RFloat, RInt),
    Register (Register),
    RegisterKind (SavedReg),
    buildRTM,
    savedReg,
    stackReg,
    updateRT,
    (#!!),
 )
import Syntax (Loc, dummyLoc)
import Prelude hiding (lookup)

data RegRefugeContext = RegRefugeContext
    { localVarsMapping :: RegMultiple (Map RegID Int)
    , loaded :: RegMultiple (Set RegID)
    , localVarsNum :: Int
    }
    deriving (Show, Eq)

type RegRefugeState = State RegRefugeContext

-- | Generate a local variable for a register.
genLocalVar :: RegType a -> RegID -> RegRefugeState Int
genLocalVar rTy regID = do
    var <- gets $ \ctx -> localVarsNum ctx
    modify $ \ctx ->
        ctx
            { localVarsMapping = updateRT rTy (insert regID var) $ localVarsMapping ctx
            , localVarsNum = 1 + localVarsNum ctx
            }
    pure var

fromLocalVar :: RegType a -> Loc -> RegID -> RegRefugeState (Maybe VirtualInst)
fromLocalVar rTy loc regID = do
    mapping <- gets $ \ctx -> localVarsMapping ctx #!! rTy
    load <- gets $ \ctx -> loaded ctx #!! rTy
    case (lookup regID mapping, member regID load) of
        (Just var, False) -> do
            -- Is bound to a local variable and has not been loaded yet.

            -- Mark as loaded.
            modify $ \ctx -> ctx{loaded = updateRT rTy (S.insert regID) $ loaded ctx}
            pure $ Just $ ILoad loc (savedReg rTy regID) stackReg var
        _ -> pure Nothing

resetLoaded :: RegRefugeState ()
resetLoaded = modify $ \ctx -> ctx{loaded = mempty}

runRefugeBlock :: LivenessGraph -> VirtualBlockGraph
runRefugeBlock graph =
    BlockGraph refuged (entryBlock graph) localNum
  where
    (refuged, RegRefugeContext _ _ localNum) =
        runState
            ( mapM
                ( \block -> do
                    refugedBlock <- refugeBlock block

                    -- Reset the context.
                    modify $ \ctx -> ctx{localVarsMapping = mempty, loaded = mempty}
                    pure refugedBlock
                )
                (graphBlocks graph)
            )
            $ RegRefugeContext mempty mempty
            $ localVars graph

-- | Gets the liveness information of the last instruction.
lastLivings :: LivenessBlock -> RegMultiple Liveness
lastLivings block =
    case unsnoc $ blockInst block of
        Just (_, inst) -> livenessProp $ getIState inst
        Nothing -> mempty

refugeBlock :: LivenessBlock -> RegRefugeState VirtualBlock
refugeBlock block = do
    inst <- concat <$> mapM refugeInst (blockInst block)

    -- Get back the registers that are still bound to local variables.
    epilogue <-
        ( buildRTM $ \rTy -> do
            stillBound <- gets $ \ctx -> fromList $ keys (localVarsMapping ctx #!! rTy)

            -- The registers can be out-of-scope.
            let shouldBeRestored = toList $ stillBound `intersection` alive (lastLivings' #!! rTy)
            catMaybes <$> mapM (fromLocalVar rTy dummyLoc) shouldBeRestored
        ) ::
            RegRefugeState (RegMultiple [VirtualInst])
    let newInst = inst <> epilogue #!! RInt <> epilogue #!! RFloat
    pure $ CodeBlock (blockName block) newInst (prevBlocks block) (terminator block)
  where
    lastLivings' = lastLivings block

refugeInst :: LivenessInst -> RegRefugeState [VirtualInst]
refugeInst (ICall (LivenessLoc loc l) label) = do
    if "__ext_" `isPrefixOf` label
        then do
            -- The external function should care about the registers by itself.
            pure [ICall loc label]
        else do
            inst <- refugeLivings loc l
            resetLoaded
            pure $ inst <> [ICall loc label]
refugeInst (ICallReg (LivenessLoc loc l) reg@(Register RInt (SavedReg cl))) = do
    -- The closure can be bound to a local variable.
    prologue <- fromLocalVar RInt loc cl
    inst <- refugeLivings loc l
    resetLoaded
    pure $ maybeToList prologue <> inst <> [ICallReg loc reg]
refugeInst (ICallReg (LivenessLoc loc l) reg) = do
    -- The closure cannot be bound to a local variable.
    inst <- refugeLivings loc l
    resetLoaded
    pure $ inst <> [ICallReg loc reg]
refugeInst inst = do
    generated <-
        ( buildRTM $ \rTy -> do
            let (InOutSet inSet _) = inOut #!! rTy
            catMaybes <$> mapM (fromLocalVar rTy loc) (toList inSet)
        ) ::
            RegRefugeState (RegMultiple [VirtualInst])
    pure $ generated #!! RInt <> generated #!! RFloat <> [substIState (const loc) inst]
  where
    loc = livenessLoc $ getIState inst
    inOut = inOutRegisters inst

-- | Refuges all registers that are not bound to local variables.
refugeLivings :: Loc -> RegMultiple Liveness -> RegRefugeState [VirtualInst]
refugeLivings loc l = do
    generated <-
        ( buildRTM $ \rTy -> do
            let (Liveness alive') = l #!! rTy
            mapping <- gets $ \ctx -> localVarsMapping ctx #!! rTy
            let shouldBeRefuged = toList $ S.filter (`notMember` mapping) alive'
            mapM
                ( \reg -> do
                    local <- genLocalVar rTy reg
                    pure $ IStore loc (savedReg rTy reg) stackReg local
                )
                shouldBeRefuged
        ) ::
            RegRefugeState (RegMultiple [VirtualInst])
    pure $ generated #!! RInt <> generated #!! RFloat
