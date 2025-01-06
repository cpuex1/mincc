{-# LANGUAGE GADTs #-}

module BackEnd.Optim.MulElim (elimMul) where

import BackEnd.Asm (
    AllowBranch,
    Inst (IBranch, IIntOp),
    IntermediateCodeBlock (getICBInst),
    PrimitiveIntOp (PDiv, PMul, PShiftL, PShiftR),
    RegOrImm (Imm),
 )

log2 :: Int -> Maybe Int
log2 1 = Just 0
log2 n
    | n <= 0 = Nothing
    | n `mod` 2 == 1 = Nothing
    | otherwise = (+ 1) <$> log2 (n `div` 2)

elimMul :: IntermediateCodeBlock stateTy idTy -> IntermediateCodeBlock stateTy idTy
elimMul block =
    block{getICBInst = map elimMul' $ getICBInst block}
  where
    elimMul' :: Inst stateTy idTy AllowBranch -> Inst stateTy idTy AllowBranch
    elimMul' (IIntOp state PMul dest src (Imm i)) =
        case log2 i of
            Just e -> IIntOp state PShiftL dest src (Imm e)
            Nothing -> IIntOp state PMul dest src (Imm i)
    elimMul' (IIntOp state PDiv dest src (Imm i)) =
        case log2 i of
            Just e -> IIntOp state PShiftR dest src (Imm e)
            Nothing -> IIntOp state PDiv dest src (Imm i)
    elimMul' (IBranch state op operand1 operand2 left right) =
        IBranch state op operand1 operand2 (map elimMul' left) (map elimMul' right)
    elimMul' inst = inst
