{-# LANGUAGE GADTs #-}

module BackEnd.Optim.Merging (mergeBlocks) where

import BackEnd.Analysis.CodeBlock (fillInPrevBlocks)
import BackEnd.Optim.Common (BackEndOptimStateT)
import CodeBlock (BlockGraph (graphBlocks), CodeBlock (CodeBlock, blockInst, blockName), Terminator (TJmp), lookupBlock)
import Data.Map (delete, insert, lookup)
import IR (Inst (IPhi), InstLabel)
import Prelude hiding (lookup)

mergeBlocks :: (Monad m) => BlockGraph a -> BackEndOptimStateT m (BlockGraph a)
mergeBlocks graph =
    pure $ fillInPrevBlocks graph{graphBlocks = mergeBlockLoop (graphBlocks graph)}
  where
    mergeBlockLoop :: [CodeBlock a] -> [CodeBlock a]
    mergeBlockLoop [] = []
    mergeBlockLoop (block@(CodeBlock beforeLabel beforeInst prev (TJmp nextLabel)) : remains) =
        case nextBlock of
            Just (CodeBlock _ afterInst [_] term) ->
                -- The next block has only one predecessor.
                let newBlock = CodeBlock beforeLabel (beforeInst ++ afterInst) prev term
                 in mergeBlockLoop (newBlock : substituted)
            _ -> block : mergeBlockLoop remains
      where
        nextBlock = lookupBlock nextLabel remains
        removed = filter (\block' -> blockName block' /= nextLabel) remains
        substituted =
            map
                ( \block' -> block'{blockInst = map (substPhi nextLabel beforeLabel) $ blockInst block'}
                )
                removed

        substPhi :: InstLabel -> InstLabel -> Inst ty -> Inst ty
        substPhi from to inst@(IPhi state dest srcs) =
            case lookup from srcs of
                Just reg -> IPhi state dest $ insert to reg $ delete from srcs
                Nothing -> inst
        substPhi _ _ inst = inst
    mergeBlockLoop (block : remains) = block : mergeBlockLoop remains
