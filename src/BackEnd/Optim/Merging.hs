module BackEnd.Optim.Merging (mergeBlocks) where

import BackEnd.Optim.Common (BackEndOptimStateT)
import CodeBlock (BlockGraph (blocks), CodeBlock (CodeBlock, blockName), Terminator (TJmp), lookupBlock)

mergeBlocks :: (Monad m) => BlockGraph a -> BackEndOptimStateT m (BlockGraph a)
mergeBlocks graph =
    pure $ graph{blocks = mergeBlockLoop (blocks graph)}
  where
    mergeBlockLoop :: [CodeBlock a] -> [CodeBlock a]
    mergeBlockLoop [] = []
    mergeBlockLoop (block@(CodeBlock beforeLabel beforeInst prev (TJmp nextLabel)) : remains) =
        case nextBlock of
            Just (CodeBlock _ afterInst [_] term) ->
                -- The next block has only one predecessor.
                let newBlock = CodeBlock beforeLabel (beforeInst ++ afterInst) prev term
                 in mergeBlockLoop (newBlock : removed)
            _ -> block : mergeBlockLoop remains
      where
        nextBlock = lookupBlock nextLabel remains
        removed = filter (\block' -> blockName block' /= nextLabel) remains
    mergeBlockLoop (block : remains) = block : mergeBlockLoop remains
