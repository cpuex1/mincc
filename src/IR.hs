{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module IR (
    RegID,
    InstLabel,
    Register (..),
    RegOrImm (Reg, Imm),
    PrimitiveIntOp (..),
    fromIntBinOp,
    IntermediateCodeBlock (..),
    CodeBlock (CodeBlock),
    exitBlock,
    InstTerm (Return, Jmp, Branch, Nop),
    AllowBranch,
    DisallowBranch,
    RawInstRetTy (..),
    Inst (..),
    getIState,
    getAllIState,
    substIState,
    replaceIReg,
    replaceFReg,
) where

import Data.Text (Text)
import Syntax (FloatBinOp, IntBinOp (..), Loc, RelationBinOp)

type RegID = Int
type InstLabel = Text

data Register idTy ty where
    ZeroReg :: Register idTy ty
    ReturnReg :: Register idTy Int
    RetReg :: Register idTy ty
    HeapReg :: Register idTy Int
    StackReg :: Register idTy Int
    ArgsReg :: idTy -> Register idTy ty
    TempReg :: idTy -> Register idTy ty
    SavedReg :: idTy -> Register idTy ty
    GeneralReg :: idTy -> Register idTy ty
    DummyReg :: Register idTy ()

deriving instance (Show idTy, Show ty) => Show (Register idTy ty)
deriving instance (Eq idTy, Eq ty) => Eq (Register idTy ty)

data RegOrImm idTy ty where
    Reg :: Register idTy ty -> RegOrImm idTy ty
    Imm :: ty -> RegOrImm idTy ty

deriving instance (Show idTy, Show ty) => Show (RegOrImm idTy ty)
deriving instance (Eq idTy, Eq ty) => Eq (RegOrImm idTy ty)

data PrimitiveIntOp
    = PAdd
    | PSub
    | PMul
    | PDiv
    | PShiftL
    | PShiftR
    | PAnd
    | POr
    | PXor
    deriving (Show, Eq)

fromIntBinOp :: IntBinOp -> PrimitiveIntOp
fromIntBinOp Add = PAdd
fromIntBinOp Sub = PSub
fromIntBinOp Mul = PMul
fromIntBinOp Div = PDiv

data IntermediateCodeBlock stateTy idTy
    = IntermediateCodeBlock
    { getICBLabel :: InstLabel
    , localVars :: Int
    , getICBInst :: [Inst stateTy idTy AllowBranch]
    }
    deriving (Show, Eq)

data CodeBlock stateTy idTy
    = CodeBlock
        InstLabel
        [Inst stateTy idTy DisallowBranch]
        (InstTerm stateTy idTy)
    deriving (Show, Eq)

data InstTerm stateTy idTy
    = Return
    | Jmp InstLabel
    | Branch
        stateTy
        RelationBinOp
        (Register idTy Int)
        (Register idTy Int)
        InstLabel
    | Nop
    deriving (Show, Eq)

exitBlock :: CodeBlock Loc Int
exitBlock = CodeBlock "__exit" [] Nop

data AllowBranch = AllowBranch
    deriving (Show, Eq)
data DisallowBranch = DisallowBranch
    deriving (Show, Eq)

data RawInstRetTy idTy = RIRUnit | RIRInt (Register idTy Int) | RIRFloat (Register idTy Float)
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
        PrimitiveIntOp ->
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
    ILMov ::
        stateTy ->
        Register idTy Int ->
        InstLabel ->
        Inst stateTy idTy DisallowBranch
    IRichCall ::
        stateTy ->
        InstLabel ->
        [RegOrImm idTy Int] ->
        [Register idTy Float] ->
        Inst stateTy idTy AllowBranch
    IClosureCall ::
        stateTy ->
        Register idTy Int ->
        [RegOrImm idTy Int] ->
        [Register idTy Float] ->
        Inst stateTy idTy AllowBranch
    IMakeClosure ::
        stateTy ->
        Register idTy Int ->
        InstLabel ->
        [RegOrImm idTy Int] ->
        [Register idTy Float] ->
        Inst stateTy idTy AllowBranch
    ICall ::
        stateTy ->
        InstLabel ->
        Inst stateTy idTy DisallowBranch
    ICallReg ::
        stateTy ->
        Register idTy Int ->
        Inst stateTy idTy DisallowBranch
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
    IRawInst ::
        stateTy ->
        Text ->
        RawInstRetTy idTy ->
        [RegOrImm idTy Int] ->
        [Register idTy Float] ->
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
getIState (ILMov state _ _) = state
getIState (IRichCall state _ _ _) = state
getIState (IClosureCall state _ _ _) = state
getIState (IMakeClosure state _ _ _ _) = state
getIState (ICall state _) = state
getIState (ICallReg state _) = state
getIState (ILoad state _ _ _) = state
getIState (IStore state _ _ _) = state
getIState (IFLoad state _ _ _) = state
getIState (IFStore state _ _ _) = state
getIState (IRawInst state _ _ _ _) = state
getIState (IBranch state _ _ _ _ _) = state

getAllIState :: Inst stateTy idTy branchTy -> [stateTy]
getAllIState (IBranch state _ _ _ thenInst elseInst) =
    state : (concatMap getAllIState thenInst ++ concatMap getAllIState elseInst)
getAllIState inst = [getIState inst]

substIState :: (stateTy -> stateTy') -> Inst stateTy idTy branchTy -> Inst stateTy' idTy branchTy
substIState f (ICompOp state op dest src1 src2) = ICompOp (f state) op dest src1 src2
substIState f (IFCompOp state op dest src1 src2) = IFCompOp (f state) op dest src1 src2
substIState f (IIntOp state op dest src1 src2) = IIntOp (f state) op dest src1 src2
substIState f (IFOp state op dest src1 src2) = IFOp (f state) op dest src1 src2
substIState f (IMov state dest src) = IMov (f state) dest src
substIState f (IFMov state dest src) = IFMov (f state) dest src
substIState f (ILMov state dest src) = ILMov (f state) dest src
substIState f (IRichCall state label iArgs fArgs) = IRichCall (f state) label iArgs fArgs
substIState f (IClosureCall state dest iArgs fArgs) = IClosureCall (f state) dest iArgs fArgs
substIState f (IMakeClosure state dest label iArgs fArgs) = IMakeClosure (f state) dest label iArgs fArgs
substIState f (ICall state label) = ICall (f state) label
substIState f (ICallReg state reg) = ICallReg (f state) reg
substIState f (ILoad state dest src offset) = ILoad (f state) dest src offset
substIState f (IStore state dest src offset) = IStore (f state) dest src offset
substIState f (IFLoad state dest src offset) = IFLoad (f state) dest src offset
substIState f (IFStore state dest src offset) = IFStore (f state) dest src offset
substIState f (IRawInst state inst retTy iArgs fArgs) = IRawInst (f state) inst retTy iArgs fArgs
substIState f (IBranch state op left right thenExpr elseExpr) =
    IBranch
        (f state)
        op
        left
        right
        ( map
            (substIState f)
            thenExpr
        )
        ( map
            (substIState f)
            elseExpr
        )

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
replaceIReg beforeReg afterReg (ILMov state dest src) =
    ILMov state (substReg' dest) src
  where
    substReg' = substReg beforeReg afterReg
replaceIReg beforeReg afterReg (IRichCall state label iArgs fArgs) =
    IRichCall state label (map substImmReg' iArgs) fArgs
  where
    substImmReg' = substImmReg beforeReg afterReg
replaceIReg beforeReg afterReg (IClosureCall state dest iArgs fArgs) =
    IClosureCall state (substReg' dest) (map substImmReg' iArgs) fArgs
  where
    substReg' = substReg beforeReg afterReg
    substImmReg' = substImmReg beforeReg afterReg
replaceIReg beforeReg afterReg (IMakeClosure state dest label iArgs fArgs) =
    IMakeClosure state (substReg' dest) label (map substImmReg' iArgs) fArgs
  where
    substReg' = substReg beforeReg afterReg
    substImmReg' = substImmReg beforeReg afterReg
replaceIReg beforeReg afterReg (ICallReg state reg) =
    ICallReg state (substReg' reg)
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
replaceIReg beforeReg afterReg (IRawInst state inst retTy iArgs fArgs) =
    case retTy of
        RIRUnit -> IRawInst state inst retTy (map substImmReg' iArgs) fArgs
        RIRInt reg -> IRawInst state inst (RIRInt (substReg' reg)) (map substImmReg' iArgs) fArgs
        RIRFloat reg -> IRawInst state inst (RIRFloat reg) (map substImmReg' iArgs) fArgs
  where
    substReg' = substReg beforeReg afterReg
    substImmReg' = substImmReg beforeReg afterReg
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
replaceIReg _ _ (IFOp state op dest left right) = IFOp state op dest left right
replaceIReg _ _ (IFMov state dest src) = IFMov state dest src
replaceIReg _ _ (ICall state label) = ICall state label

replaceFReg ::
    (Eq idTy) =>
    Register idTy Float ->
    Register idTy Float ->
    Inst stateTy idTy branchTy ->
    Inst stateTy idTy branchTy
replaceFReg beforeReg afterReg (IFCompOp state op dest left right) =
    IFCompOp state op dest (substReg' left) (substReg' right)
  where
    substReg' = substReg beforeReg afterReg
replaceFReg beforeReg afterReg (IFOp state op dest left right) =
    IFOp state op (substReg' dest) (substReg' left) (substReg' right)
  where
    substReg' = substReg beforeReg afterReg
replaceFReg beforeReg afterReg (IFMov state dest src) =
    IFMov state (substReg' dest) (substImmReg' src)
  where
    substReg' = substReg beforeReg afterReg
    substImmReg' = substImmReg beforeReg afterReg
replaceFReg beforeReg afterReg (IRichCall state label iArgs fArgs) =
    IRichCall state label iArgs (map substReg' fArgs)
  where
    substReg' = substReg beforeReg afterReg
replaceFReg beforeReg afterReg (IClosureCall state dest iArgs fArgs) =
    IClosureCall state dest iArgs (map substReg' fArgs)
  where
    substReg' = substReg beforeReg afterReg
replaceFReg beforeReg afterReg (IMakeClosure state dest label iArgs fArgs) =
    IMakeClosure state dest label iArgs (map substReg' fArgs)
  where
    substReg' = substReg beforeReg afterReg
replaceFReg beforeReg afterReg (IFLoad state dest src offset) =
    IFLoad state (substReg' dest) src offset
  where
    substReg' = substReg beforeReg afterReg
replaceFReg beforeReg afterReg (IFStore state dest src offset) =
    IFStore state (substReg' dest) src offset
  where
    substReg' = substReg beforeReg afterReg
replaceFReg beforeReg afterReg (IRawInst state inst retTy iArgs fArgs) =
    case retTy of
        RIRUnit -> IRawInst state inst retTy iArgs (map substReg' fArgs)
        RIRInt reg -> IRawInst state inst (RIRInt reg) iArgs (map substReg' fArgs)
        RIRFloat reg -> IRawInst state inst (RIRFloat (substReg' reg)) iArgs (map substReg' fArgs)
  where
    substReg' = substReg beforeReg afterReg
replaceFReg beforeReg afterReg (IBranch state op left right thenExpr elseExpr) =
    IBranch
        state
        op
        left
        right
        ( map
            (replaceFReg beforeReg afterReg)
            thenExpr
        )
        ( map
            (replaceFReg beforeReg afterReg)
            elseExpr
        )
replaceFReg _ _ inst = inst
