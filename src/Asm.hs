{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Asm (
    InstLabel,
    Register (
        ZeroReg,
        RetReg,
        HeapReg,
        StackReg,
        ArgsReg,
        TempReg,
        DummyReg
    ),
    RegOrImm (Reg, Imm),
    IntermediateCodeBlock (IntermediateCodeBlock),
    CodeBlock (CodeBlock),
    InstTerm (Return, Jmp, Branch, Nop),
    AllowBranch,
    DisallowBranch,
    Inst (
        ICompOp,
        IFCompOp,
        IIntOp,
        IFOp,
        IMov,
        IFMov,
        IRichCall,
        IClosureCall,
        IMakeClosure,
        ICall,
        ILoad,
        IStore,
        IFLoad,
        IFStore,
        IBranch,
        IRet,
        IFRet
    ),
) where

import Data.Text (Text)
import Syntax (FloatBinOp, IntBinOp, RelationBinOp)

type InstLabel = Text

data Register idTy ty where
    ZeroReg :: Register idTy ty
    RetReg :: Register idTy ty
    HeapReg :: Register idTy Int
    StackReg :: Register idTy Int
    ArgsReg :: idTy -> Register idTy ty
    TempReg :: idTy -> Register idTy ty
    DummyReg :: Register idTy ()

deriving instance (Show idTy, Show ty) => Show (Register idTy ty)
deriving instance (Eq idTy, Eq ty) => Eq (Register idTy ty)

data RegOrImm idTy ty where
    Reg :: Register idTy ty -> RegOrImm idTy ty
    Imm :: ty -> RegOrImm idTy ty

deriving instance (Show idTy, Show ty) => Show (RegOrImm idTy ty)
deriving instance (Eq idTy, Eq ty) => Eq (RegOrImm idTy ty)

data IntermediateCodeBlock stateTy idTy
    = IntermediateCodeBlock
        InstLabel
        [Inst stateTy idTy AllowBranch]
    deriving (Show, Eq)

data CodeBlock stateTy idTy
    = CodeBlock
        InstLabel
        [Inst stateTy idTy DisallowBranch]
        (InstTerm stateTy idTy)
    deriving (Show, Eq)

data InstTerm stateTy idTy
    = Return stateTy
    | Jmp stateTy InstLabel
    | Branch
        stateTy
        RelationBinOp
        (Register idTy Int)
        (Register idTy Int)
        InstLabel
        InstLabel
    | Nop
    deriving (Show, Eq)

data AllowBranch = AllowBranch
    deriving (Show, Eq)
data DisallowBranch = DisallowBranch
    deriving (Show, Eq)

data Inst stateTy idTy branchTy where
    ICompOp ::
        stateTy ->
        RelationBinOp ->
        Register idTy Int ->
        Register idTy Int ->
        RegOrImm idTy Int ->
        Inst stateTy idTy branchTy
    IFCompOp ::
        stateTy ->
        RelationBinOp ->
        Register idTy Int ->
        Register idTy Float ->
        Register idTy Float ->
        Inst stateTy idTy branchTy
    IIntOp ::
        stateTy ->
        IntBinOp ->
        Register idTy Int ->
        Register idTy Int ->
        RegOrImm idTy Int ->
        Inst stateTy idTy branchTy
    IFOp ::
        stateTy ->
        FloatBinOp ->
        Register idTy Float ->
        Register idTy Float ->
        Register idTy Float ->
        Inst stateTy idTy branchTy
    IMov ::
        stateTy ->
        Register idTy Int ->
        RegOrImm idTy Int ->
        Inst stateTy idTy branchTy
    IFMov ::
        stateTy ->
        Register idTy Float ->
        RegOrImm idTy Float ->
        Inst stateTy idTy branchTy
    IRichCall ::
        stateTy ->
        InstLabel ->
        [Register idTy Int] ->
        [Register idTy Float] ->
        Inst stateTy idTy AllowBranch
    IClosureCall ::
        stateTy ->
        Register idTy Int ->
        [Register idTy Int] ->
        [Register idTy Float] ->
        Inst stateTy idTy AllowBranch
    IMakeClosure ::
        stateTy ->
        Register idTy Int ->
        InstLabel ->
        [Register idTy Int] ->
        [Register idTy Float] ->
        Inst stateTy idTy AllowBranch
    ICall ::
        stateTy ->
        InstLabel ->
        Inst stateTy idTy branchTy
    ILoad ::
        stateTy ->
        Register idTy Int ->
        Register idTy Int ->
        Int ->
        Inst stateTy idTy branchTy
    IStore ::
        stateTy ->
        Register idTy Int ->
        Register idTy Int ->
        Int ->
        Inst stateTy idTy branchTy
    IFLoad ::
        stateTy ->
        Register idTy Float ->
        Register idTy Int ->
        Int ->
        Inst stateTy idTy branchTy
    IFStore ::
        stateTy ->
        Register idTy Float ->
        Register idTy Int ->
        Int ->
        Inst stateTy idTy branchTy
    IBranch ::
        stateTy ->
        RelationBinOp ->
        Register idTy Int ->
        Register idTy Int ->
        [Inst stateTy idTy AllowBranch] ->
        [Inst stateTy idTy AllowBranch] ->
        Inst stateTy idTy AllowBranch
    IRet ::
        stateTy ->
        Register idTy Int ->
        Inst stateTy idTy AllowBranch
    IFRet ::
        stateTy ->
        Register idTy Float ->
        Inst stateTy idTy AllowBranch

deriving instance (Show stateTy, Show idTy, Show branchTy) => Show (Inst stateTy idTy branchTy)
deriving instance (Eq stateTy, Eq idTy, Eq branchTy) => Eq (Inst stateTy idTy branchTy)
