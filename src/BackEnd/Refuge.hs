{-# LANGUAGE GADTs #-}

module BackEnd.Refuge (runRefugeBlock) where

import BackEnd.Analysis.IR (InOutSet (InOutSet), inOutRegisters)
import BackEnd.Liveness (
    Liveness (Liveness),
    LivenessBlock,
    LivenessGraph,
    LivenessInst,
    LivenessLoc (LivenessLoc, livenessLoc),
 )
import CodeBlock (BlockGraph (..), CodeBlock (..), VirtualBlock, VirtualBlockGraph)
import Control.Monad.State (State, gets, modify, runState)
import Data.Foldable (toList)
import Data.Map (Map, delete, insert, keys, lookup, notMember)
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.Set as S
import IR (
    AbstInst,
    Inst (..),
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

fromLocalVar :: RegType a -> Loc -> RegID -> RegRefugeState (Maybe AbstInst)
fromLocalVar rTy loc regID = do
    mapping <- gets $ \ctx -> localVarsMapping ctx #!! rTy
    case lookup regID mapping of
        Just var -> do
            -- Bound to a local variable.

            -- Remove the local variable from the mapping.
            modify $ \ctx -> ctx{localVarsMapping = updateRT rTy (delete regID) $ localVarsMapping ctx}
            pure $ Just $ ILoad loc (savedReg rTy regID) stackReg var
        Nothing -> pure Nothing

runRefugeBlock :: LivenessGraph -> VirtualBlockGraph
runRefugeBlock graph =
    BlockGraph refuged (entryBlock graph) localNum
  where
    (refuged, RegRefugeContext _ localNum) =
        runState
            ( mapM
                ( \block -> do
                    refugedBlock <- refugeBlock block
                    modify $ \ctx -> ctx{localVarsMapping = mempty}
                    pure refugedBlock
                )
                (graphBlocks graph)
            )
            $ RegRefugeContext mempty
            $ localVars graph

refugeBlock :: LivenessBlock -> RegRefugeState VirtualBlock
refugeBlock block = do
    inst <- concat <$> mapM refugeInst (blockInst block)

    -- Get back the registers that are still bound to local variables.
    epilogue <-
        ( buildRTM $ \rTy -> do
            stillBound <- gets $ \ctx -> keys (localVarsMapping ctx #!! rTy)
            catMaybes <$> mapM (fromLocalVar rTy dummyLoc) stillBound
        ) ::
            RegRefugeState (RegMultiple [AbstInst])
    let newInst = inst <> epilogue #!! RInt <> epilogue #!! RFloat
    pure $ CodeBlock (blockName block) newInst (prevBlocks block) (terminator block)

refugeInst :: LivenessInst -> RegRefugeState [AbstInst]
refugeInst (ICall (LivenessLoc loc l) label) = do
    inst <- refugeLivings loc l
    pure $ inst <> [ICall loc label]
refugeInst (ICallReg (LivenessLoc loc l) reg@(Register RInt (SavedReg cl))) = do
    -- The closure can be bound to a local variable.
    prologue <- fromLocalVar RInt loc cl
    inst <- refugeLivings loc l
    pure $ maybeToList prologue <> inst <> [ICallReg loc reg]
refugeInst (ICallReg (LivenessLoc loc l) reg) = do
    -- The closure cannot be bound to a local variable.
    inst <- refugeLivings loc l
    pure $ inst <> [ICallReg loc reg]
refugeInst inst = do
    generated <-
        ( buildRTM $ \rTy -> do
            let (InOutSet inSet _) = inOut #!! rTy
            catMaybes <$> mapM (fromLocalVar rTy loc) (toList inSet)
        ) ::
            RegRefugeState (RegMultiple [AbstInst])
    pure $ generated #!! RInt <> generated #!! RFloat <> [substIState (const loc) inst]
  where
    loc = livenessLoc $ getIState inst
    inOut = inOutRegisters inst

-- | Refuges all registers that are not bound to local variables.
refugeLivings :: Loc -> RegMultiple Liveness -> RegRefugeState [AbstInst]
refugeLivings loc l = do
    generated <-
        ( buildRTM $ \rTy -> do
            let (Liveness alive) = l #!! rTy
            mapping <- gets $ \ctx -> localVarsMapping ctx #!! rTy
            let shouldBeRefuged = toList $ S.filter (`notMember` mapping) alive
            mapM
                ( \reg -> do
                    local <- genLocalVar rTy reg
                    pure $ IStore loc (savedReg rTy reg) stackReg local
                )
                shouldBeRefuged
        ) ::
            RegRefugeState (RegMultiple [AbstInst])
    pure $ generated #!! RInt <> generated #!! RFloat
