{-# LANGUAGE GADTs #-}

module BackEnd.Optim.Merging (mergeBlocks) where

import BackEnd.Optim.Common (BackEndOptimStateT, renameLabels)
import CodeBlock (
    BlockGraph (graphBlocks),
    CodeBlock (CodeBlock, blockName),
    Terminator (TJmp),
    VirtualBlock,
    VirtualBlockGraph,
    lookupBlock,
 )
import Data.Map (Map, insert, lookup)
import Data.Maybe (fromMaybe)
import IR (InstLabel)
import Prelude hiding (lookup)

mergeBlocks :: (Monad m) => VirtualBlockGraph -> BackEndOptimStateT m VirtualBlockGraph
mergeBlocks graph =
    let (merged, nameDict) = mergeBlockLoop (graphBlocks graph)
     in let newGraph = graph{graphBlocks = merged}
         in pure $
                renameLabels
                    ( \label -> fromMaybe label (lookup label nameDict)
                    )
                    newGraph
  where
    mergeBlockLoop :: [VirtualBlock] -> ([VirtualBlock], Map InstLabel InstLabel)
    mergeBlockLoop [] = ([], mempty)
    mergeBlockLoop (block@(CodeBlock beforeLabel beforeInst prev (TJmp nextLabel)) : remains) =
        case nextBlock of
            Just (CodeBlock _ afterInst [_] term) ->
                -- The next block has only one predecessor.
                let newBlock = CodeBlock beforeLabel (beforeInst ++ afterInst) prev term
                 in let (remains'', nameDict') = mergeBlockLoop (newBlock : removed)
                     in (remains'', insert nextLabel beforeLabel nameDict')
            _ -> (block : remains', nameDict)
      where
        nextBlock = lookupBlock nextLabel remains
        removed = filter (\block' -> blockName block' /= nextLabel) remains
        (remains', nameDict) = mergeBlockLoop remains
    mergeBlockLoop (block : remains) = (block : remains', nameDict)
      where
        (remains', nameDict) = mergeBlockLoop remains
