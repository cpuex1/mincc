{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module BackEnd.RegisterAlloc (assignReg) where

import BackEnd.Algorithm.Graph (coloringByDegree)
import BackEnd.Analysis.Phi (phiGroups, toPhiMapping)
import BackEnd.Liveness (
    Liveness (Liveness),
    LivenessGraph,
    LivenessLoc (livenessLoc),
    allLivenessInfo,
    constructGraph,
 )
import CodeBlock (
    CodeBlock (CodeBlock, blockInst, blockName, prevBlocks, terminator),
    PhiFreeBlock,
    PhiFreeGraph,
    VirtualBlock,
    VirtualBlockGraph,
    mapRegGraph,
    visitBlock,
    visitInst,
 )
import Control.Monad.Identity (Identity (runIdentity))
import Data.Map (elems, lookup)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import IR (
    AbstInst,
    Inst (..),
    PhiFreeInst,
    substIState,
 )
import Registers (
    RegID,
    RegMapping (regMap),
    RegVariant,
    Register (Register),
    RegisterKind (SavedReg),
    applyMapping,
    createVariant,
    savedReg,
    zeroReg,
    (#!!),
    (#$),
 )
import Prelude hiding (lookup)

data RegAllocResult a = RegAllocResult
    { numReg :: Int
    , spillTarget :: Maybe RegID
    , allocated :: RegMapping a
    }

-- | Perform register allocation.
allocateReg :: LivenessGraph -> RegVariant RegAllocResult
allocateReg graph = result
  where
    -- Get the liveness information.
    livenessInfo = allLivenessInfo graph

    -- Calculate register mapping coming from the phi instructions.
    phiMapping = toPhiMapping $ phiGroups graph

    -- Apply the mapping to the liveness information.
    reflected = map (applyMappingToLiveness phiMapping) livenessInfo

    -- Perform register coloring.
    regGraph = constructGraph reflected
    mapping = const coloringByDegree #$ regGraph

    -- Combine two mappings.
    finalMapping = mapping <> phiMapping

    -- Create the result.
    result = createVariant $ \rTy ->
        RegAllocResult
            { numReg =
                1 + foldl max (-1) (elems $ regMap $ finalMapping #!! rTy)
            , spillTarget = Nothing
            , allocated = finalMapping #!! rTy
            }

-- | Perform register allocation and apply the result to the graph.
assignReg :: LivenessGraph -> (RegVariant RegAllocResult, PhiFreeGraph)
assignReg graph = (result, phiFreeGraph)
  where
    -- Allocate registers.
    result = allocateReg graph

    -- Get register mapping.
    mapping = const allocated #$ result

    -- Apply register mapping.
    mappedGraph =
        mapRegGraph
            ( \case
                Register rTy (SavedReg regId) ->
                    case lookup regId $ regMap $ mapping #!! rTy of
                        Just target -> savedReg rTy target
                        Nothing -> zeroReg rTy
                reg -> reg
            )
            graph

    -- Purge liveness information from the graph.
    purgedGraph = runIdentity (visitBlock (visitInst (pure . substIState livenessLoc)) mappedGraph) :: VirtualBlockGraph

    -- Remove phi instructions.
    phiFreeGraph = runIdentity (visitBlock (pure . removePhi) purgedGraph)

-- | Remove phi instructions from a block.
removePhi :: VirtualBlock -> PhiFreeBlock
removePhi block =
    CodeBlock
        { blockName = blockName block
        , blockInst =
            mapMaybe toPhiFree $
                blockInst block
        , prevBlocks = prevBlocks block
        , terminator = terminator block
        }
  where
    toPhiFree :: AbstInst -> Maybe PhiFreeInst
    toPhiFree (ICompOp state op dest src1 src2) = Just $ ICompOp state op dest src1 src2
    toPhiFree (IIntOp state op dest src1 src2) = Just $ IIntOp state op dest src1 src2
    toPhiFree (IFOp state op dest src1 src2) = Just $ IFOp state op dest src1 src2
    toPhiFree (ILoad state dest src offset) = Just $ ILoad state dest src offset
    toPhiFree (IStore state dest src offset) = Just $ IStore state dest src offset
    toPhiFree (IMov state dest src) = Just $ IMov state dest src
    toPhiFree (IRichCall state label iArgs fArgs) = Just $ IRichCall state label iArgs fArgs
    toPhiFree (IClosureCall state cl iArgs fArgs) = Just $ IClosureCall state cl iArgs fArgs
    toPhiFree (IMakeClosure state dest label iArgs fArgs) = Just $ IMakeClosure state dest label iArgs fArgs
    toPhiFree (IRawInst state name retTy iArgs fArgs) = Just $ IRawInst state name retTy iArgs fArgs
    toPhiFree IPhi{} = Nothing

-- | Apply the phi mapping to the liveness information.
applyMappingToLiveness :: RegVariant RegMapping -> RegVariant Liveness -> RegVariant Liveness
applyMappingToLiveness mapping live =
    (\rTy (Liveness live') -> Liveness $ S.map (applyMapping rTy mapping) live') #$ live
