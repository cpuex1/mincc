{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Asm (
    InstLabel,
    Register (ZeroReg, RetReg, ArgsReg, TempReg, DummyReg),
    Operand (Reg, Imm, Mem, DirectMem),
    CodeBlock (CodeBlock),
    InstTerm (Return, Jmp, Branch, Nop),
    Inst (InstRelationOp, InstIntBinOp, InstFloatBinOp, InstMov, InstFMov, InstCall, InstLoad, InstStore),
) where

import Data.Text (Text)
import Syntax (FloatBinOp, IntBinOp, RelationBinOp)

type InstLabel = Text

data Register idTy ty where
    ZeroReg :: Register idTy ty
    RetReg :: Register idTy ty
    ArgsReg :: idTy -> Register idTy ty
    TempReg :: idTy -> Register idTy ty
    DummyReg :: Register idTy ()

deriving instance (Show idTy, Show ty) => Show (Register idTy ty)
deriving instance (Eq idTy, Eq ty) => Eq (Register idTy ty)

data Operand idTy ty where
    Reg :: Register idTy ty -> Operand idTy ty
    Imm :: ty -> Operand idTy ty
    Mem :: Register idTy Int -> Int -> Operand idTy ty
    DirectMem :: Int -> Operand idTy ty

deriving instance (Show idTy, Show ty) => Show (Operand idTy ty)
deriving instance (Eq idTy, Eq ty) => Eq (Operand idTy ty)

data CodeBlock stateTy regTy
    = CodeBlock InstLabel [Inst stateTy regTy] (InstTerm stateTy regTy)
    deriving (Show, Eq)

data InstTerm stateTy regTy
    = Return stateTy
    | Jmp stateTy InstLabel
    | Branch stateTy RelationBinOp (Operand Int regTy) (Operand Int regTy) InstLabel InstLabel
    | Nop
    deriving (Show, Eq)

data Inst stateTy idTy where
    InstRelationOp ::
        stateTy ->
        RelationBinOp ->
        Operand idTy Int ->
        Operand idTy Int ->
        Operand idTy Int ->
        Inst stateTy idTy
    InstIntBinOp ::
        stateTy ->
        IntBinOp ->
        Operand idTy Int ->
        Operand idTy Int ->
        Operand idTy Int ->
        Inst stateTy idTy
    InstFloatBinOp ::
        stateTy ->
        FloatBinOp ->
        Operand idTy Float ->
        Operand idTy Float ->
        Operand idTy Float ->
        Inst stateTy idTy
    InstMov ::
        stateTy ->
        Operand idTy Int ->
        Operand idTy Int ->
        Inst stateTy idTy
    InstFMov ::
        stateTy ->
        Operand idTy Float ->
        Operand idTy Float ->
        Inst stateTy idTy
    InstCall ::
        stateTy ->
        InstLabel ->
        Inst stateTy idTy
    InstLoad ::
        stateTy ->
        Operand idTy Int ->
        Operand idTy Int ->
        Inst stateTy idTy
    InstStore ::
        stateTy ->
        Operand idTy Int ->
        Operand idTy Int ->
        Inst stateTy idTy

deriving instance (Show stateTy, Show idTy) => Show (Inst stateTy idTy)
deriving instance (Eq stateTy, Eq idTy) => Eq (Inst stateTy idTy)
