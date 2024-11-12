{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Backend.Asm (
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
        IBranch
    ),
    getIState,
    replaceIReg,
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

deriving instance (Show stateTy, Show idTy, Show branchTy) => Show (Inst stateTy idTy branchTy)
deriving instance (Eq stateTy, Eq idTy, Eq branchTy) => Eq (Inst stateTy idTy branchTy)

getIState :: Inst stateTy idTy branchTy -> stateTy
getIState (ICompOp state _ _ _ _) = state
getIState (IFCompOp state _ _ _ _) = state
getIState (IIntOp state _ _ _ _) = state
getIState (IFOp state _ _ _ _) = state
getIState (IMov state _ _) = state
getIState (IFMov state _ _) = state
getIState (IRichCall state _ _ _) = state
getIState (IClosureCall state _ _ _) = state
getIState (IMakeClosure state _ _ _ _) = state
getIState (ICall state _) = state
getIState (ILoad state _ _ _) = state
getIState (IStore state _ _ _) = state
getIState (IFLoad state _ _ _) = state
getIState (IFStore state _ _ _) = state
getIState (IBranch state _ _ _ _ _) = state

substReg :: (Eq idTy, Eq ty) => Register idTy ty -> Register idTy ty -> Register idTy ty -> Register idTy ty
substReg beforeReg afterReg victim =
    if victim == beforeReg then afterReg else victim

substImmReg :: (Eq idTy, Eq ty) => Register idTy ty -> Register idTy ty -> RegOrImm idTy ty -> RegOrImm idTy ty
substImmReg beforeReg afterReg victim =
    if victim == Reg beforeReg then Reg afterReg else victim

replaceIReg ::
    (Eq idTy) =>
    Register idTy Int ->
    Register idTy Int ->
    Inst stateTy idTy branchTy ->
    Inst stateTy idTy branchTy
replaceIReg beforeReg afterReg (ICompOp state op dest left right) =
    ICompOp state op (substReg' dest) (substReg' left) (substImmReg' right)
  where
    substReg' = substReg beforeReg afterReg
    substImmReg' = substImmReg beforeReg afterReg
replaceIReg beforeReg afterReg (IFCompOp state op dest left right) =
    IFCompOp state op (substReg' dest) left right
  where
    substReg' = substReg beforeReg afterReg
replaceIReg beforeReg afterReg (IIntOp state op dest left right) =
    IIntOp state op (substReg' dest) (substReg' left) (substImmReg' right)
  where
    substReg' = substReg beforeReg afterReg
    substImmReg' = substImmReg beforeReg afterReg
replaceIReg beforeReg afterReg (IMov state dest src) =
    IMov state (substReg' dest) (substImmReg' src)
  where
    substReg' = substReg beforeReg afterReg
    substImmReg' = substImmReg beforeReg afterReg
replaceIReg beforeReg afterReg (IRichCall state label iArgs fArgs) =
    IRichCall state label (map substReg' iArgs) fArgs
  where
    substReg' = substReg beforeReg afterReg
replaceIReg beforeReg afterReg (IClosureCall state dest iArgs fArgs) =
    IClosureCall state (substReg' dest) (map substReg' iArgs) fArgs
  where
    substReg' = substReg beforeReg afterReg
replaceIReg beforeReg afterReg (IMakeClosure state dest label iArgs fArgs) =
    IMakeClosure state (substReg' dest) label (map substReg' iArgs) fArgs
  where
    substReg' = substReg beforeReg afterReg
replaceIReg beforeReg afterReg (ILoad state dest src offset) =
    ILoad state (substReg' dest) (substReg' src) offset
  where
    substReg' = substReg beforeReg afterReg
replaceIReg beforeReg afterReg (IFLoad state dest src offset) =
    IFLoad state dest (substReg' src) offset
  where
    substReg' = substReg beforeReg afterReg
replaceIReg beforeReg afterReg (IStore state dest src offset) =
    IStore state (substReg' dest) (substReg' src) offset
  where
    substReg' = substReg beforeReg afterReg
replaceIReg beforeReg afterReg (IFStore state dest src offset) =
    IFStore state dest (substReg' src) offset
  where
    substReg' = substReg beforeReg afterReg
replaceIReg beforeReg afterReg (IBranch state op left right thenExpr elseExpr) =
    IBranch
        state
        op
        (substReg' left)
        (substReg' right)
        ( map
            (replaceIReg beforeReg afterReg)
            thenExpr
        )
        ( map
            (replaceIReg beforeReg afterReg)
            elseExpr
        )
  where
    substReg' = substReg beforeReg afterReg
replaceIReg _ _ inst = inst
