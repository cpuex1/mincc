{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module BackEnd.Optim.EmptyBlockMerging (
    mergeEmptyBlockM,
) where

import BackEnd.Analysis.CodeBlock (fillInPrevBlocks)
import BackEnd.Optim.Common (BackEndOptimStateT, renameLabels, updatePhi)
import CodeBlock (BlockGraph (graphBlocks), CodeBlock (blockInst, blockName, prevBlocks, terminator), Terminator (TJmp), VirtualBlock, VirtualBlockGraph, lookupBlock, visitBlock)
import Data.Map (Map, delete, insert, lookup, union)
import Data.Maybe (mapMaybe)
import Data.Set (disjoint, fromList)
import Debug.Trace (traceShow)
import IR (Inst (IPhi), InstLabel, VirtualInst)
import Registers (RegTuple (createRT), RegVariant, Register (Register), updateRT, (#!!))
import Prelude hiding (lookup)

newtype PhiInfo a = PhiInfo
    { phiInfo :: Map (Register a) (Map InstLabel (Register a))
    }

collectPhi :: VirtualBlock -> Maybe (RegVariant PhiInfo)
collectPhi block =
    foldl
        ( \info inst ->
            case (info, inst) of
                (Just info', IPhi _ dest@(Register rTy _) srcs) ->
                    Just $ updateRT rTy (\(PhiInfo info'') -> PhiInfo $ insert dest srcs info'') info'
                _ -> Nothing
        )
        (Just (createRT (PhiInfo mempty) (PhiInfo mempty)))
        (blockInst block)

-- | Modify the phi instruction.
modifyPhiInst :: InstLabel -> [InstLabel] -> RegVariant PhiInfo -> VirtualInst -> VirtualInst
modifyPhiInst label prevLabels info (IPhi state dest@(Register rTy _) srcs) =
    case lookup label srcs of
        Just src ->
            case lookup src (phiInfo $ info #!! rTy) of
                Just srcs' -> IPhi state dest $ union srcs' $ delete label srcs
                _ ->
                    IPhi state dest $ foldl (\srcs' label' -> insert label' src srcs') (delete label srcs) prevLabels
        Nothing ->
            error (traceShow (label, srcs) "The relation of two blocks is not valid.")
modifyPhiInst _ _ _ inst = inst

mergeEmptyBlockM :: (Monad m) => VirtualBlockGraph -> BackEndOptimStateT m VirtualBlockGraph
mergeEmptyBlockM graph = do
    let graph' = mergeEmptyBlock graph
    visitBlock (pure . updatePhi) graph'

mergeEmptyBlock :: VirtualBlockGraph -> VirtualBlockGraph
mergeEmptyBlock graph =
    maybe graph mergeEmptyBlock modified
  where
    blocks = graphBlocks graph
    modified =
        foldl
            ( \g b ->
                case g of
                    Just _ -> g
                    Nothing -> mergeEmptyBlockWithTarget b graph
            )
            Nothing
            blocks

mergeEmptyBlockWithTarget :: VirtualBlock -> VirtualBlockGraph -> Maybe VirtualBlockGraph
mergeEmptyBlockWithTarget block graph =
    let phiInfo' = collectPhi block
     in case (phiInfo', terminator block) of
            (Just phiInfo'', TJmp next) ->
                let nextBlock = lookupBlock next $ graphBlocks graph
                 in case nextBlock of
                        Just nextBlock' ->
                            if fromList (prevBlocks nextBlock') `disjoint` fromList (prevBlocks block)
                                then
                                    let modified = map (modifyPhiInst (blockName block) (prevBlocks block) phiInfo'') $ blockInst nextBlock'
                                     in let modifiedNext = traceShow (blockName block, next) nextBlock'{blockInst = modified}
                                         in let newBlocks =
                                                    mapMaybe
                                                        ( \block' ->
                                                            if block' == nextBlock'
                                                                then Just modifiedNext
                                                                else
                                                                    if block' == block
                                                                        then Nothing
                                                                        else Just block'
                                                        )
                                                        $ graphBlocks graph
                                             in Just $
                                                    fillInPrevBlocks $
                                                        renameLabels (\l -> if l == blockName block then next else l) $
                                                            graph{graphBlocks = newBlocks}
                                else
                                    -- If the block would have been removed, the phi instructions can contradict.
                                    Nothing
                        _ -> error "The next block does not exist."
            _ ->
                Nothing
