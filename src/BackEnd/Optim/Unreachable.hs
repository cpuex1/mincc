module BackEnd.Optim.Unreachable (removeUnreachable) where

import BackEnd.Analysis.CodeBlock (fillInPrevBlocks)
import BackEnd.Optim.Common (BackEndOptimStateT, updatePhi)
import CodeBlock (BlockGraph (BlockGraph, graphBlocks), CodeBlock (blockName, prevBlocks))

-- | Remove unreachable blocks.
removeUnreachable :: (Monad m) => BlockGraph a -> BackEndOptimStateT m (BlockGraph a)
removeUnreachable (BlockGraph blocks' entryLabel localVars) =
    pure $ filled{graphBlocks = map updatePhi (graphBlocks filled)}
  where
    graph =
        BlockGraph
            ( filter
                ( \block ->
                    not (null $ prevBlocks block) || blockName block == entryLabel
                )
                blocks'
            )
            entryLabel
            localVars
    filled = fillInPrevBlocks graph
