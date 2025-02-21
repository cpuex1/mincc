{-# LANGUAGE GADTs #-}

module BackEnd.Optim.MulElim (elimMul) where

import BackEnd.Optim.Common (BackEndOptimStateT)
import CodeBlock (BlockGraph (graphBlocks), VirtualBlockGraph, blockInst)
import IR (
    Inst (IIntOp),
    PrimitiveIntOp (PDiv, PMul, PShiftL, PShiftR),
 )
import Registers (RegOrImm (Imm), RegType (RInt))

log2 :: Int -> Maybe Int
log2 1 = Just 0
log2 n
    | n <= 0 = Nothing
    | n `mod` 2 == 1 = Nothing
    | otherwise = (+ 1) <$> log2 (n `div` 2)

elimMul :: (Monad m) => VirtualBlockGraph -> BackEndOptimStateT m VirtualBlockGraph
elimMul graph =
    pure $ graph{graphBlocks = map (\block -> block{blockInst = map elimMul' (blockInst block)}) $ graphBlocks graph}
  where
    elimMul' :: Inst ty -> Inst ty
    elimMul' (IIntOp state PMul dest src (Imm _ i)) =
        case log2 i of
            Just e -> IIntOp state PShiftL dest src (Imm RInt e)
            Nothing -> IIntOp state PMul dest src (Imm RInt i)
    elimMul' (IIntOp state PDiv dest src (Imm _ i)) =
        case log2 i of
            Just e -> IIntOp state PShiftR dest src (Imm RInt e)
            Nothing -> IIntOp state PDiv dest src (Imm RInt i)
    elimMul' inst = inst
