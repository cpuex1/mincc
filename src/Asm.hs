{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Asm () where

import Data.Text (Text)
import Syntax (FloatBinOp, IntBinOp, RelationBinOp)

newtype InstLabel
    = InstLabel Text
    deriving (Show, Eq)

data OInt = OInt
    deriving (Show, Eq)

data OFloat = OFloat
    deriving (Show, Eq)

data Operand opTy regTy where
    Reg :: regTy -> Operand opTy regTy
    Imm :: opTy -> Operand opTy regTy
    Mem :: regTy -> Int -> Operand opTy regTy

deriving instance (Show opTy, Show regTy) => Show (Operand opTy regTy)
deriving instance (Eq opTy, Eq regTy) => Eq (Operand opTy regTy)

data CodeBlock stateTy regTy
    = CodeBlock InstLabel [Inst stateTy regTy] (InstTerm stateTy regTy)
    deriving Show

data InstTerm stateTy regTy
    = Return stateTy
    | Jmp stateTy InstLabel
    | Branch stateTy RelationBinOp (Operand OInt regTy) (Operand OInt regTy) InstLabel InstLabel
    | Nop stateTy
    deriving (Show, Eq)

data Inst stateTy regTy where
    InstRelationOp ::
        stateTy ->
        RelationBinOp ->
        Operand OInt regTy ->
        Operand OInt regTy ->
        Operand OInt regTy ->
        Inst stateTy regTy
    InstIntBinOp ::
        stateTy ->
        IntBinOp ->
        Operand OInt regTy ->
        Operand OInt regTy ->
        Operand OInt regTy ->
        Inst stateTy regTy
    InstFloatOp ::
        stateTy ->
        FloatBinOp ->
        Operand OFloat regTy ->
        Operand OFloat regTy ->
        Operand OFloat regTy ->
        Inst stateTy regTy
    InstMov ::
        (Show opTy, Eq opTy) =>
        stateTy ->
        Operand opTy regTy ->
        Operand opTy regTy ->
        Inst stateTy regTy
    InstCall ::
        stateTy ->
        InstLabel ->
        Inst stateTy regTy
    InstLoad ::
        stateTy ->
        Operand OInt regTy ->
        Operand OInt regTy ->
        Inst stateTy regTy
    InstStore ::
        stateTy ->
        Operand OInt regTy ->
        Operand OInt regTy ->
        Inst stateTy regTy

deriving instance (Show stateTy, Show regTy) => Show (Inst stateTy regTy)
