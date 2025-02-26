module BackEnd.CodeGen.Optim (removeMeaninglessAssignment) where

import CodeBlock (CodeBlock (blockInst), PhiFreeBlock, PhiFreeGraph, visitBlock)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Maybe (mapMaybe)
import IR (Inst (IMov), PhiFreeInst)
import Registers (RegOrImm (Reg))

-- Struggling

-- | Remove the instructions that assign a register to itself.
removeMeaninglessAssignment :: PhiFreeGraph -> PhiFreeGraph
removeMeaninglessAssignment graph =
    runIdentity $ visitBlock (pure . removeInstInBlock) graph
  where
    removeInstInBlock :: PhiFreeBlock -> PhiFreeBlock
    removeInstInBlock block =
        block{blockInst = mapMaybe removeInst (blockInst block)}

    removeInst :: PhiFreeInst -> Maybe PhiFreeInst
    removeInst inst@(IMov _ dest (Reg src)) =
        if dest == src
            then
                Nothing
            else
                Just inst
    removeInst inst = Just inst
