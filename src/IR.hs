{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module IR (
    RegID,
    InstLabel,
    PrimitiveIntOp (..),
    fromIntBinOp,
    HCodeBlock (..),
    LCodeBlock (..),
    exitBlock,
    InstTerm (Return, Jmp, Branch, Nop),
    isTerm,
    Inst (..),
    AbstInst,
    AbstCodeBlock,
    RawInst,
    RawCodeBlock,
    RawInstTerm,
    InstKind (..),
    getIState,
    getAllIState,
    substIState,
    mapReg,
    replaceReg,
) where

import Data.Kind (Type)
import Data.Text (Text)
import Registers (RegOrImm (Imm, Reg), RegType (RFloat, RInt), Register (Register))
import Syntax (FloatBinOp, IntBinOp (..), Loc, RelationBinOp)

type RegID = Int
type InstLabel = Text

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

-- | Higher-level code block.
data HCodeBlock ty where
    HCodeBlock ::
        (AllowInstBranch ty ~ True) =>
        { hLabel :: InstLabel
        , localVars :: Int
        , hInst :: [Inst ty]
        } ->
        HCodeBlock ty

deriving instance (Show (Inst ty)) => Show (HCodeBlock ty)
deriving instance (Eq (Inst ty)) => Eq (HCodeBlock ty)

-- | Lower-level code block.
data LCodeBlock ty where
    LCodeBlock ::
        (AllowInstBranch ty ~ False) =>
        { lGetLabel :: InstLabel
        , lInst :: [Inst ty]
        , lTerm :: InstTerm ty
        } ->
        LCodeBlock ty

deriving instance (Show (Inst ty), Show (InstTerm ty)) => Show (LCodeBlock ty)
deriving instance (Eq (Inst ty), Eq (InstTerm ty)) => Eq (LCodeBlock ty)

-- | The last block to be executed.
exitBlock :: (AllowInstBranch ty ~ False) => LCodeBlock ty
exitBlock = LCodeBlock "__exit" [] Nop

data InstTerm ty where
    Return :: InstTerm ty
    Jmp :: InstLabel -> InstTerm ty
    Branch ::
        (InstStateTy ty) ->
        RelationBinOp ->
        (Register (RegIDTy ty) a) ->
        (Register (RegIDTy ty) a) ->
        InstLabel ->
        InstTerm ty
    Nop :: InstTerm ty

isTerm :: InstTerm ty -> Bool
isTerm Nop = False
isTerm _ = True

class InstKind ty where
    type InstStateTy ty :: Type
    type RegIDTy ty :: Type
    type AllowInstBranch ty :: Bool

data AbstInstKind

instance InstKind AbstInstKind where
    type InstStateTy AbstInstKind = Loc
    type RegIDTy AbstInstKind = RegID
    type AllowInstBranch AbstInstKind = True

type AbstInst = Inst AbstInstKind
type AbstCodeBlock = HCodeBlock AbstInstKind

data RawInstKind

instance InstKind RawInstKind where
    type InstStateTy RawInstKind = Loc
    type RegIDTy RawInstKind = RegID
    type AllowInstBranch RawInstKind = False

type RawInst = Inst RawInstKind
type RawCodeBlock = LCodeBlock RawInstKind
type RawInstTerm = InstTerm RawInstKind

data Inst ty where
    ICompOp ::
        InstStateTy ty ->
        RelationBinOp ->
        Register (RegIDTy ty) Int ->
        Register (RegIDTy ty) a ->
        RegOrImm (RegIDTy ty) a ->
        Inst ty
    IIntOp ::
        InstStateTy ty ->
        PrimitiveIntOp ->
        Register (RegIDTy ty) Int ->
        Register (RegIDTy ty) Int ->
        RegOrImm (RegIDTy ty) Int ->
        Inst ty
    IFOp ::
        InstStateTy ty ->
        FloatBinOp ->
        Register (RegIDTy ty) Float ->
        Register (RegIDTy ty) Float ->
        Register (RegIDTy ty) Float ->
        Inst ty
    IMov ::
        InstStateTy ty ->
        Register (RegIDTy ty) a ->
        RegOrImm (RegIDTy ty) a ->
        Inst ty
    ILMov ::
        (AllowInstBranch ty ~ False) =>
        InstStateTy ty ->
        Register (RegIDTy ty) Int ->
        InstLabel ->
        Inst ty
    IRichCall ::
        (AllowInstBranch ty ~ True) =>
        InstStateTy ty ->
        InstLabel ->
        [RegOrImm (RegIDTy ty) Int] ->
        [Register (RegIDTy ty) Float] ->
        Inst ty
    IClosureCall ::
        (AllowInstBranch ty ~ True) =>
        InstStateTy ty ->
        Register (RegIDTy ty) Int ->
        [RegOrImm (RegIDTy ty) Int] ->
        [Register (RegIDTy ty) Float] ->
        Inst ty
    IMakeClosure ::
        (AllowInstBranch ty ~ True) =>
        InstStateTy ty ->
        Register (RegIDTy ty) Int ->
        InstLabel ->
        [RegOrImm (RegIDTy ty) Int] ->
        [Register (RegIDTy ty) Float] ->
        Inst ty
    ICall ::
        (AllowInstBranch ty ~ False) =>
        InstStateTy ty ->
        InstLabel ->
        Inst ty
    ICallReg ::
        (AllowInstBranch ty ~ False) =>
        InstStateTy ty ->
        Register (RegIDTy ty) Int ->
        Inst ty
    ILoad ::
        InstStateTy ty ->
        Register (RegIDTy ty) a ->
        Register (RegIDTy ty) Int ->
        Int ->
        Inst ty
    IStore ::
        InstStateTy ty ->
        Register (RegIDTy ty) a ->
        Register (RegIDTy ty) Int ->
        Int ->
        Inst ty
    IRawInst ::
        InstStateTy ty ->
        Text ->
        Register (RegIDTy ty) a ->
        [RegOrImm (RegIDTy ty) Int] ->
        [Register (RegIDTy ty) Float] ->
        Inst ty
    IBranch ::
        (AllowInstBranch ty ~ True) =>
        InstStateTy ty ->
        RelationBinOp ->
        Register (RegIDTy ty) a ->
        Register (RegIDTy ty) a ->
        [Inst ty] ->
        [Inst ty] ->
        Inst ty

instance (Eq (InstStateTy ty), Eq (RegIDTy ty)) => Eq (Inst ty) where
    (ICompOp state1 op1 dest1 src1_1@(Register RInt _) src1_2) == (ICompOp state2 op2 dest2 src2_1@(Register RInt _) src2_2) =
        state1 == state2 && op1 == op2 && dest1 == dest2 && src1_1 == src2_1 && src1_2 == src2_2
    (ICompOp state1 op1 dest1 src1_1@(Register RFloat _) src1_2) == (ICompOp state2 op2 dest2 src2_1@(Register RFloat _) src2_2) =
        state1 == state2 && op1 == op2 && dest1 == dest2 && src1_1 == src2_1 && src1_2 == src2_2
    (IIntOp state1 op1 dest1 src1_1 src1_2) == (IIntOp state2 op2 dest2 src2_1 src2_2) =
        state1 == state2 && op1 == op2 && dest1 == dest2 && src1_1 == src2_1 && src1_2 == src2_2
    (IFOp state1 op1 dest1 src1_1 src1_2) == (IFOp state2 op2 dest2 src2_1 src2_2) =
        state1 == state2 && op1 == op2 && dest1 == dest2 && src1_1 == src2_1 && src1_2 == src2_2
    (IMov state1 dest1@(Register RInt _) src1) == (IMov state2 dest2@(Register RInt _) src2) =
        state1 == state2 && dest1 == dest2 && src1 == src2
    (IMov state1 dest1@(Register RFloat _) src1) == (IMov state2 dest2@(Register RFloat _) src2) =
        state1 == state2 && dest1 == dest2 && src1 == src2
    (ILMov state1 dest1 label1) == (ILMov state2 dest2 label2) =
        state1 == state2 && dest1 == dest2 && label1 == label2
    (IRichCall state1 label1 iArgs1 fArgs1) == (IRichCall state2 label2 iArgs2 fArgs2) =
        state1 == state2 && label1 == label2 && iArgs1 == iArgs2 && fArgs1 == fArgs2
    (IClosureCall state1 dest1 iArgs1 fArgs1) == (IClosureCall state2 dest2 iArgs2 fArgs2) =
        state1 == state2 && dest1 == dest2 && iArgs1 == iArgs2 && fArgs1 == fArgs2
    (IMakeClosure state1 dest1 label1 iArgs1 fArgs1) == (IMakeClosure state2 dest2 label2 iArgs2 fArgs2) =
        state1 == state2 && dest1 == dest2 && label1 == label2 && iArgs1 == iArgs2 && fArgs1 == fArgs2
    (ICall state1 label1) == (ICall state2 label2) =
        state1 == state2 && label1 == label2
    (ICallReg state1 reg1) == (ICallReg state2 reg2) =
        state1 == state2 && reg1 == reg2
    (ILoad state1 dest1@(Register RInt _) src1 offset1) == (ILoad state2 dest2@(Register RInt _) src2 offset2) =
        state1 == state2 && dest1 == dest2 && src1 == src2 && offset1 == offset2
    (ILoad state1 dest1@(Register RFloat _) src1 offset1) == (ILoad state2 dest2@(Register RFloat _) src2 offset2) =
        state1 == state2 && dest1 == dest2 && src1 == src2 && offset1 == offset2
    (IStore state1 dest1@(Register RInt _) src1 offset1) == (IStore state2 dest2@(Register RInt _) src2 offset2) =
        state1 == state2 && dest1 == dest2 && src1 == src2 && offset1 == offset2
    (IStore state1 dest1@(Register RFloat _) src1 offset1) == (IStore state2 dest2@(Register RFloat _) src2 offset2) =
        state1 == state2 && dest1 == dest2 && src1 == src2 && offset1 == offset2
    (IRawInst state1 inst1 retTy1@(Register RInt _) iArgs1 fArgs1) == (IRawInst state2 inst2 retTy2@(Register RInt _) iArgs2 fArgs2) =
        state1 == state2 && inst1 == inst2 && retTy1 == retTy2 && iArgs1 == iArgs2 && fArgs1 == fArgs2
    (IRawInst state1 inst1 retTy1@(Register RFloat _) iArgs1 fArgs1) == (IRawInst state2 inst2 retTy2@(Register RFloat _) iArgs2 fArgs2) =
        state1 == state2 && inst1 == inst2 && retTy1 == retTy2 && iArgs1 == iArgs2 && fArgs1 == fArgs2
    (IBranch state1 op1 left1@(Register RInt _) right1 thenExpr1 elseExpr1) == (IBranch state2 op2 left2@(Register RInt _) right2 thenExpr2 elseExpr2) =
        state1 == state2 && op1 == op2 && left1 == left2 && right1 == right2 && thenExpr1 == thenExpr2 && elseExpr1 == elseExpr2
    (IBranch state1 op1 left1@(Register RFloat _) right1 thenExpr1 elseExpr1) == (IBranch state2 op2 left2@(Register RFloat _) right2 thenExpr2 elseExpr2) =
        state1 == state2 && op1 == op2 && left1 == left2 && right1 == right2 && thenExpr1 == thenExpr2 && elseExpr1 == elseExpr2
    _ == _ = False

getIState :: Inst ty -> InstStateTy ty
getIState (ICompOp state _ _ _ _) = state
getIState (IIntOp state _ _ _ _) = state
getIState (IFOp state _ _ _ _) = state
getIState (IMov state _ _) = state
getIState (ILMov state _ _) = state
getIState (IRichCall state _ _ _) = state
getIState (IClosureCall state _ _ _) = state
getIState (IMakeClosure state _ _ _ _) = state
getIState (ICall state _) = state
getIState (ICallReg state _) = state
getIState (ILoad state _ _ _) = state
getIState (IStore state _ _ _) = state
getIState (IRawInst state _ _ _ _) = state
getIState (IBranch state _ _ _ _ _) = state

getAllIState :: Inst ty -> [InstStateTy ty]
getAllIState (IBranch state _ _ _ thenInst elseInst) =
    state : (concatMap getAllIState thenInst ++ concatMap getAllIState elseInst)
getAllIState inst = [getIState inst]

substIState ::
    (RegIDTy a ~ RegIDTy b, AllowInstBranch a ~ AllowInstBranch b) =>
    (InstStateTy a -> InstStateTy b) ->
    Inst a ->
    Inst b
substIState f (ICompOp state op dest src1 src2) = ICompOp (f state) op dest src1 src2
substIState f (IIntOp state op dest src1 src2) = IIntOp (f state) op dest src1 src2
substIState f (IFOp state op dest src1 src2) = IFOp (f state) op dest src1 src2
substIState f (IMov state dest src) = IMov (f state) dest src
substIState f (ILMov state dest src) = ILMov (f state) dest src
substIState f (IRichCall state label iArgs fArgs) = IRichCall (f state) label iArgs fArgs
substIState f (IClosureCall state dest iArgs fArgs) = IClosureCall (f state) dest iArgs fArgs
substIState f (IMakeClosure state dest label iArgs fArgs) = IMakeClosure (f state) dest label iArgs fArgs
substIState f (ICall state label) = ICall (f state) label
substIState f (ICallReg state reg) = ICallReg (f state) reg
substIState f (ILoad state dest src offset) = ILoad (f state) dest src offset
substIState f (IStore state dest src offset) = IStore (f state) dest src offset
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

weakSubstReg :: (Eq idTy, Eq ty) => Register idTy ty -> Register idTy ty -> Register idTy ty -> Register idTy ty
weakSubstReg beforeReg afterReg victim =
    if victim == beforeReg then afterReg else victim

substReg :: (Eq idTy) => Register idTy ty1 -> Register idTy ty1 -> Register idTy ty2 -> Register idTy ty2
substReg (Register RInt before) afterReg (Register RInt victim) =
    weakSubstReg (Register RInt before) afterReg (Register RInt victim)
substReg (Register RFloat before) afterReg (Register RFloat victim) =
    weakSubstReg (Register RFloat before) afterReg (Register RFloat victim)
substReg _ _ victim = victim

coverImm :: (Register idTy ty -> Register idTy ty) -> (RegOrImm idTy ty -> RegOrImm idTy ty)
coverImm _ (Imm rTy imm) = Imm rTy imm
coverImm substR (Reg reg) = Reg $ substR reg

mapReg ::
    (forall regTy1. Register (RegIDTy ty) regTy1 -> Register (RegIDTy ty) regTy1) ->
    Inst ty ->
    Inst ty
mapReg substR (ICompOp state op dest left right) =
    ICompOp state op (substR dest) (substR left) (coverImm substR right)
mapReg substR (IIntOp state op dest left right) =
    IIntOp state op (substR dest) (substR left) (coverImm substR right)
mapReg substR (IFOp state op dest left right) =
    IFOp state op (substR dest) (substR left) (substR right)
mapReg substR (IMov state dest src) =
    IMov state (substR dest) (coverImm substR src)
mapReg substR (ILMov state dest src) =
    ILMov state (substR dest) src
mapReg substR (IRichCall state label iArgs fArgs) =
    IRichCall state label (map (coverImm substR) iArgs) (map substR fArgs)
mapReg substR (IClosureCall state dest iArgs fArgs) =
    IClosureCall state (substR dest) (map (coverImm substR) iArgs) (map substR fArgs)
mapReg substR (IMakeClosure state dest label iArgs fArgs) =
    IMakeClosure state (substR dest) label (map (coverImm substR) iArgs) (map substR fArgs)
mapReg _ (ICall state label) =
    ICall state label
mapReg substR (ICallReg state reg) =
    ICallReg state (substR reg)
mapReg substR (ILoad state dest src offset) =
    ILoad state (substR dest) (substR src) offset
mapReg substR (IStore state dest src offset) =
    IStore state (substR dest) (substR src) offset
mapReg substR (IRawInst state inst retReg iArgs fArgs) =
    IRawInst state inst (substR retReg) (map (coverImm substR) iArgs) (map substR fArgs)
mapReg substR (IBranch state op left right thenExpr elseExpr) =
    IBranch
        state
        op
        (substR left)
        (substR right)
        ( map
            (mapReg substR)
            thenExpr
        )
        ( map
            (mapReg substR)
            elseExpr
        )

replaceReg ::
    (Eq (RegIDTy ty)) =>
    Register (RegIDTy ty) regTy ->
    Register (RegIDTy ty) regTy ->
    Inst ty ->
    Inst ty
replaceReg before after = mapReg (substReg before after)
