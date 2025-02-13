module BackEnd.Optim.Unreachable (removeUnreachable) where

import BackEnd.Analysis.CodeBlock (fillInPrevBlocks)
import BackEnd.Optim.Common (BackEndOptimStateT, updatePhi)
import CodeBlock (BlockGraph (BlockGraph, blocks), CodeBlock (blockName, prevBlocks))

-- | Remove unreachable blocks.
removeUnreachable :: (Monad m) => BlockGraph a -> BackEndOptimStateT m (BlockGraph a)
removeUnreachable (BlockGraph blocks' entryLabel) =
    pure $ filled{blocks = map updatePhi (blocks filled)}
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
    filled = fillInPrevBlocks graph
