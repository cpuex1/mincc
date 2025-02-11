module BackEnd.Optim.Unreachable (removeUnreachable) where

import BackEnd.Optim.Common (BackEndOptimStateT)
import CodeBlock (BlockGraph (BlockGraph), CodeBlock (blockName, prevBlocks))

-- | Remove unreachable blocks.
removeUnreachable :: (Monad m) => BlockGraph a -> BackEndOptimStateT m (BlockGraph a)
removeUnreachable (BlockGraph blocks entryLabel) =
    pure $
        BlockGraph
            ( filter
                ( \block ->
                    not (null $ prevBlocks block) || blockName block == entryLabel
                )
                blocks
            )
            entryLabel
