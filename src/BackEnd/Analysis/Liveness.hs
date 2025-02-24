{-# LANGUAGE GADTs #-}

module BackEnd.Analysis.Liveness (
    runGraphLiveness,
) where

import BackEnd.Analysis.IR (InOutSet (inSet, outSet), inOutRegisters)
import BackEnd.Liveness (Liveness (Liveness, alive), LivenessBlock, LivenessGraph, LivenessInst, LivenessLoc (LivenessLoc))
import CodeBlock (
    BlockGraph (BlockGraph, entryBlock, graphBlocks, localVars),
    CodeBlock (..),
    Terminator (TBranch),
    VirtualBlock,
    VirtualBlockGraph,
    lookupBlock,
    nextBlocks,
 )
import Control.Monad.State (State, gets, modify, runState)
import Data.Map (Map, lookup)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (delete, insert, singleton, union, (\\))
import IR (
    Inst (..),
    InstLabel,
    VirtualInst,
    getIState,
    substIState,
 )
import Registers (
    RegMultiple,
    RegTuple (createRT),
    RegType (RFloat, RInt),
    Register (Register),
    RegisterKind (SavedReg),
    updateRT,
    (#!!),
    (#$),
 )
import Syntax (Loc)
import Prelude hiding (lookup)

-- | Holds liveness information.
data LivenessContext = LivenessContext
    { currentLiveness :: RegMultiple Liveness
    , analyzed :: Map InstLabel (RegMultiple Liveness)
    , prevAnalyzed :: Map InstLabel (RegMultiple Liveness)
    }
    deriving (Show, Eq)

type LivenessState = State LivenessContext

markUsed :: Register a -> LivenessState ()
markUsed (Register rTy (SavedReg regId)) = do
    modify $ \ctx ->
        ctx
            { currentLiveness =
                updateRT rTy (Liveness . insert regId . alive) $
                    currentLiveness ctx
            }
markUsed _ = pure ()

remove :: Register a -> LivenessState ()
remove (Register rTy (SavedReg regId)) = do
    modify $ \ctx ->
        ctx
            { currentLiveness =
                updateRT rTy (Liveness . delete regId . alive) $
                    currentLiveness ctx
            }
remove _ = pure ()

getState :: Loc -> LivenessState LivenessLoc
getState loc' =
    gets $ LivenessLoc loc' . currentLiveness

runGraphLiveness :: VirtualBlockGraph -> LivenessGraph
runGraphLiveness graph = runGraphLiveness' mempty
  where
    runGraphLiveness' :: Map InstLabel (RegMultiple Liveness) -> LivenessGraph
    runGraphLiveness' prevAnalyzed' =
        if analyzed ctx == prevAnalyzed'
            then
                -- The liveness converges.
                livenessGraph
            else
                -- The liveness can be updated.
                runGraphLiveness' $ analyzed ctx
      where
        (livenessGraph, ctx) = runState (graphLiveness graph) (LivenessContext mempty mempty prevAnalyzed')

graphLiveness :: VirtualBlockGraph -> LivenessState LivenessGraph
graphLiveness graph = do
    -- Visit each block in reverse order.
    blocks' <- reverse <$> mapM (blockLiveness graph) (reverse $ graphBlocks graph)
    pure $ BlockGraph blocks' (entryBlock graph) (localVars graph)

-- | Get the liveness information from its label.
livenessFromLabel :: InstLabel -> LivenessState (RegMultiple Liveness)
livenessFromLabel label = do
    live <- gets (lookup label . analyzed)
    case live of
        Just l -> pure l
        Nothing -> do
            live' <- gets (lookup label . prevAnalyzed)
            case live' of
                Just l -> pure l
                Nothing -> pure mempty

blockLiveness :: VirtualBlockGraph -> VirtualBlock -> LivenessState LivenessBlock
blockLiveness graph block = do
    -- Retrieve the liveness information from next blocks.
    nextLiveness <- mconcat <$> mapM livenessFromLabel next
    let startLiveness = nextLiveness <> phiLiveness
    modify $ \ctx -> ctx{currentLiveness = startLiveness}

    -- Visit the terminator instruction.
    terminatorLiveness $ terminator block

    -- Visit each instruction in the block in reverse order.
    instWithLiveness <- reverse <$> mapM instLiveness (reverse $ blockInst block)

    -- Save the liveness information for the previous blocks.
    l <- gets currentLiveness
    modify $ \ctx -> ctx{analyzed = M.insert (blockName block) l (analyzed ctx)}

    pure $ CodeBlock (blockName block) instWithLiveness (prevBlocks block) (terminator block)
  where
    next = nextBlocks block
    nextBlocks' = mapMaybe (`lookupBlock` graphBlocks graph) next

    -- Consider phi instructions.
    phiLiveness = mconcat $ map (\b -> phiFromLabel (blockInst b) (blockName block)) nextBlocks'

terminatorLiveness :: Terminator -> LivenessState ()
terminatorLiveness (TBranch _ lhs rhs _ _) = do
    markUsed lhs
    markUsed rhs
terminatorLiveness _ = pure ()

instLiveness :: VirtualInst -> LivenessState LivenessInst
instLiveness (IPhi state dest srcs) = do
    -- We should avoid to mark source registers as used
    -- because it should be resolved at higher level.
    state' <- getState state
    remove dest
    pure $ IPhi state' dest srcs
instLiveness inst = do
    state <- getState $ getIState inst
    modify $ \ctx ->
        ctx
            { currentLiveness =
                ( \rTy (Liveness live) ->
                    -- The liveness information should be updated as follows:
                    -- ```
                    -- LIVE := (LIVE \ OUT) /\ IN
                    -- ```
                    let outS = outSet (inOut #!! rTy)
                     in let inS = inSet (inOut #!! rTy)
                         in Liveness $ (live \\ outS) `union` inS
                )
                    #$ currentLiveness ctx
            }
    pure $ substIState (const state) inst
  where
    inOut = inOutRegisters inst

-- | Get all registers from the label.
phiFromLabel :: [Inst ty] -> InstLabel -> RegMultiple Liveness
phiFromLabel [] _ = mempty
phiFromLabel (IPhi _ _ srcs : remains) fromLabel =
    case lookup fromLabel srcs of
        Just (Register RInt (SavedReg reg)) -> createRT (Liveness $ singleton reg) mempty <> remains'
        Just (Register RFloat (SavedReg reg)) -> createRT mempty (Liveness $ singleton reg) <> remains'
        _ -> remains'
  where
    remains' = phiFromLabel remains fromLabel
phiFromLabel (_ : remains) fromLabel = phiFromLabel remains fromLabel
