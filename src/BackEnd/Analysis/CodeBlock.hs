{-# LANGUAGE GADTs #-}

module BackEnd.Analysis.CodeBlock (
    fillInPrevBlocks,
) where

import CodeBlock (
    BlockGraph (BlockGraph),
    CodeBlock (blockName, prevBlocks),
    nextBlocks,
 )
import Data.Map (adjust, fromList, lookup)
import Prelude hiding (lookup)

-- | Fill in the previous blocks fields.
fillInPrevBlocks :: BlockGraph a -> BlockGraph a
fillInPrevBlocks (BlockGraph blocks entryLabel) =
    BlockGraph
        (map (\block -> block{prevBlocks = concat $ lookup (blockName block) previousBlocks}) blocks)
        entryLabel
  where
    emptyMap = fromList $ map (\b -> (blockName b, [])) blocks
    previousBlocks =
        foldl
            ( \prev block ->
                foldl
                    ( flip (adjust (blockName block :))
                    )
                    prev
                    $ nextBlocks block
            )
            emptyMap
            blocks
