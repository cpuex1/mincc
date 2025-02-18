{-# LANGUAGE GADTs #-}

module BackEnd.Analysis.Liveness (
    runGraphLiveness,
) where

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
import Control.Monad.State (State, evalState, gets, modify)
import Data.Map (Map, lookup)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (delete, insert, singleton)
import IR (
    AbstInst,
    Inst (..),
    InstLabel,
 )
import Registers (
    RegOrImm (Reg),
    RegType (RFloat, RInt),
    RegVariant (RegVariant),
    Register (Register),
    RegisterKind (SavedReg),
    updateVariant,
 )
import Syntax (Loc)
import Prelude hiding (lookup)

-- | Holds liveness information.
data LivenessContext = LivenessContext
    { currentLiveness :: RegVariant Liveness
    , analyzed :: Map InstLabel (RegVariant Liveness)
    }
    deriving (Show, Eq)

type LivenessState = State LivenessContext

markUsed :: Register a -> LivenessState ()
markUsed (Register rTy (SavedReg regId)) = do
    modify $ \ctx ->
        ctx
            { currentLiveness =
                updateVariant rTy (Liveness . insert regId . alive) $
                    currentLiveness ctx
            }
markUsed _ = pure ()

markUsedImm :: RegOrImm a -> LivenessState ()
markUsedImm (Reg reg) = markUsed reg
markUsedImm _ = pure ()

remove :: Register a -> LivenessState ()
remove (Register rTy (SavedReg regId)) = do
    modify $ \ctx ->
        ctx
            { currentLiveness =
                updateVariant rTy (Liveness . delete regId . alive) $
                    currentLiveness ctx
            }
remove _ = pure ()

getState :: Loc -> LivenessState LivenessLoc
getState loc' =
    gets $ LivenessLoc loc' . currentLiveness

runGraphLiveness :: VirtualBlockGraph -> LivenessGraph
runGraphLiveness graph = evalState (graphLiveness graph) (LivenessContext mempty mempty)

graphLiveness :: VirtualBlockGraph -> LivenessState LivenessGraph
graphLiveness graph = do
    -- Visit each block in reverse order.
    blocks' <- reverse <$> mapM (blockLiveness graph) (reverse $ graphBlocks graph)
    pure $ BlockGraph blocks' (entryBlock graph) (localVars graph)

blockLiveness :: VirtualBlockGraph -> VirtualBlock -> LivenessState LivenessBlock
blockLiveness graph block = do
    -- Retrieve the liveness information from next blocks.
    already <- gets analyzed
    let nextLiveness = mconcat $ mapMaybe (`lookup` already) next
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

instLiveness :: AbstInst -> LivenessState LivenessInst
instLiveness (ICompOp state op dest src1 src2) = do
    state' <- getState state
    remove dest
    markUsed src1
    markUsedImm src2
    pure $ ICompOp state' op dest src1 src2
instLiveness (IIntOp state op dest src1 src2) = do
    state' <- getState state
    remove dest
    markUsed src1
    markUsedImm src2
    pure $ IIntOp state' op dest src1 src2
instLiveness (IFOp state op dest src1 src2) = do
    state' <- getState state
    remove dest
    markUsed src1
    markUsed src2
    pure $ IFOp state' op dest src1 src2
instLiveness (IMov state dest src) = do
    state' <- getState state
    remove dest
    markUsedImm src
    pure $ IMov state' dest src
instLiveness (ICall state func) = do
    state' <- getState state
    pure $ ICall state' func
instLiveness (ICallReg state cl) = do
    state' <- getState state
    markUsed cl
    pure $ ICallReg state' cl
instLiveness (ILMov state dest label) = do
    state' <- getState state
    remove dest
    pure $ ILMov state' dest label
instLiveness (ILoad state dest src offset) = do
    state' <- getState state
    remove dest
    markUsed src
    pure $ ILoad state' dest src offset
instLiveness (IStore state dest src offset) = do
    state' <- getState state
    markUsed dest
    markUsed src
    pure $ IStore state' dest src offset
instLiveness (IRawInst state name retTy iArgs fArgs) = do
    state' <- getState state
    remove retTy
    mapM_ markUsedImm iArgs
    mapM_ markUsed fArgs
    pure $ IRawInst state' name retTy iArgs fArgs
instLiveness (IPhi state dest srcs) = do
    -- We should avoid to mark source registers as used
    -- because it should be resolved at higher level.
    state' <- getState state
    remove dest
    pure $ IPhi state' dest srcs

-- | Get all registers from the label.
phiFromLabel :: [Inst ty] -> InstLabel -> RegVariant Liveness
phiFromLabel [] _ = mempty
phiFromLabel (IPhi _ _ srcs : remains) fromLabel =
    case lookup fromLabel srcs of
        Just (Register RInt (SavedReg reg)) -> RegVariant (Liveness $ singleton reg) mempty <> remains'
        Just (Register RFloat (SavedReg reg)) -> RegVariant mempty (Liveness $ singleton reg) <> remains'
        _ -> remains'
  where
    remains' = phiFromLabel remains fromLabel
phiFromLabel (_ : remains) fromLabel = phiFromLabel remains fromLabel
