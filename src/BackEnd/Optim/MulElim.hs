{-# LANGUAGE GADTs #-}

module BackEnd.Optim.MulElim (elimMul) where

import BackEnd.Optim.Common (BackEndOptimStateT)
import IR (
    AbstCodeBlock,
    HCodeBlock (hInst),
    Inst (IBranch, IIntOp),
    PrimitiveIntOp (PDiv, PMul, PShiftL, PShiftR),
 )
import Registers (RegOrImm (Imm), RegType (RInt))

log2 :: Int -> Maybe Int
log2 1 = Just 0
log2 n
    | n <= 0 = Nothing
    | n `mod` 2 == 1 = Nothing
    | otherwise = (+ 1) <$> log2 (n `div` 2)

elimMul :: (Monad m) => AbstCodeBlock -> BackEndOptimStateT m AbstCodeBlock
elimMul block =
    pure $ block{hInst = map elimMul' $ hInst block}
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
    elimMul' (IBranch state op operand1 operand2 left right) =
        IBranch state op operand1 operand2 (map elimMul' left) (map elimMul' right)
    elimMul' inst = inst
