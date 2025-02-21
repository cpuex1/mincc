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
    InstLabel,
    PrimitiveIntOp (..),
    fromIntBinOp,
    Inst (..),
    VirtualInst,
    VirtualInstKind,
    PhiFreeInst,
    PhiFreeInstKind,
    InstKind (..),
    getIState,
    substIState,
) where

import Data.Kind (Type)
import Data.Map (Map, toAscList)
import qualified Data.Map as M
import Data.Text (Text, intercalate, justifyLeft, pack)
import Display (Display (display), DisplayI (displayI))
import Registers (RegOrImm (Imm, Reg), RegReplaceable (..), RegType (RFloat, RInt), Register (Register), coverImm)
import Syntax (FloatBinOp (..), IntBinOp (..), Loc, RelationBinOp (..))

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

class InstKind ty where
    type InstStateTy ty :: Type
    type AllowPhi ty :: Bool

data VirtualInstKind

instance InstKind VirtualInstKind where
    type InstStateTy VirtualInstKind = Loc
    type AllowPhi VirtualInstKind = True

type VirtualInst = Inst VirtualInstKind

data PhiFreeInstKind

instance InstKind PhiFreeInstKind where
    type InstStateTy PhiFreeInstKind = Loc
    type AllowPhi PhiFreeInstKind = False

type PhiFreeInst = Inst PhiFreeInstKind

data Inst ty where
    ICompOp ::
        InstStateTy ty ->
        RelationBinOp ->
        Register Int ->
        Register a ->
        RegOrImm a ->
        Inst ty
    IIntOp ::
        InstStateTy ty ->
        PrimitiveIntOp ->
        Register Int ->
        Register Int ->
        RegOrImm Int ->
        Inst ty
    IFOp ::
        InstStateTy ty ->
        FloatBinOp ->
        Register Float ->
        Register Float ->
        Register Float ->
        Inst ty
    IMov ::
        InstStateTy ty ->
        Register a ->
        RegOrImm a ->
        Inst ty
    ICall ::
        InstStateTy ty ->
        InstLabel ->
        Inst ty
    ILMov ::
        InstStateTy ty ->
        Register Int ->
        InstLabel ->
        Inst ty
    ICallReg ::
        InstStateTy ty ->
        Register Int ->
        Inst ty
    ILoad ::
        InstStateTy ty ->
        Register a ->
        Register Int ->
        Int ->
        Inst ty
    IStore ::
        InstStateTy ty ->
        Register a ->
        Register Int ->
        Int ->
        Inst ty
    IRawInst ::
        InstStateTy ty ->
        Text ->
        Register a ->
        [RegOrImm Int] ->
        [Register Float] ->
        Inst ty
    IPhi ::
        (AllowPhi ty ~ True) =>
        InstStateTy ty ->
        Register a ->
        Map InstLabel (Register a) ->
        Inst ty

deriving instance (Show (InstStateTy ty)) => Show (Inst ty)

instance (Eq (InstStateTy ty)) => Eq (Inst ty) where
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
    (IPhi state1 dest1@(Register RInt _) choices1) == (IPhi state2 dest2@(Register RInt _) choices2) =
        state1 == state2 && dest1 == dest2 && choices1 == choices2
    (IPhi state1 dest1@(Register RFloat _) choices1) == (IPhi state2 dest2@(Register RFloat _) choices2) =
        state1 == state2 && dest1 == dest2 && choices1 == choices2
    _ == _ = False

instructionWidth :: Int
instructionWidth = 30

instance (Display (InstStateTy ty)) => DisplayI (Inst ty) where
    displayI depth inst =
        justifyLeft instructionWidth ' ' displayed <> " # " <> display (getIState inst)
      where
        displayed = withoutState inst depth

        withoutState :: Inst ty -> Int -> Text
        withoutState (ICompOp _ op lhs rhs1@(Register rTy _) rhs2) _ =
            case rhs2 of
                (Reg rhs2') ->
                    prefix rTy <> toOp op <> " " <> display lhs <> ", " <> display rhs1 <> ", " <> display rhs2'
                (Imm RInt rhs2') ->
                    prefix rTy <> toOp op <> "i " <> display lhs <> ", " <> display rhs1 <> ", " <> pack (show rhs2')
                (Imm RFloat rhs2') ->
                    prefix rTy <> toOp op <> "i " <> display lhs <> ", " <> display rhs1 <> ", " <> pack (show rhs2')
          where
            prefix :: RegType a -> Text
            prefix RInt = "s"
            prefix RFloat = "f"

            toOp :: RelationBinOp -> Text
            toOp Eq = "eq"
            toOp Ne = "ne"
            toOp Ge = "ge"
            toOp Lt = "lt"
        withoutState (IIntOp _ op lhs rhs1 rhs2) _ =
            case rhs2 of
                (Reg rhs2') ->
                    toOp op <> " " <> display lhs <> ", " <> display rhs1 <> ", " <> display rhs2'
                (Imm _ rhs2') ->
                    toOp op <> "i " <> display lhs <> ", " <> display rhs1 <> ", " <> pack (show rhs2')
          where
            toOp :: PrimitiveIntOp -> Text
            toOp PAdd = "add"
            toOp PSub = "sub"
            toOp PMul = "mul"
            toOp PDiv = "div"
            toOp PAnd = "and"
            toOp POr = "or"
            toOp PXor = "xor"
            toOp PShiftL = "sll"
            toOp PShiftR = "srl"
        withoutState (IFOp _ op lhs rhs1 rhs2) _ =
            toOp op <> " " <> display lhs <> ", " <> display rhs1 <> ", " <> display rhs2
          where
            toOp :: FloatBinOp -> Text
            toOp FAdd = "fadd"
            toOp FSub = "fsub"
            toOp FMul = "fmul"
            toOp FDiv = "fdiv"
        withoutState (IMov _ lhs rhs@(Reg (Register RInt _))) _ =
            "mov " <> display lhs <> ", " <> display rhs
        withoutState (IMov _ lhs rhs@(Imm RInt _)) _ =
            "movi " <> display lhs <> ", " <> display rhs
        withoutState (IMov _ lhs rhs@(Reg (Register RFloat _))) _ =
            "fmov " <> display lhs <> ", " <> display rhs
        withoutState (IMov _ lhs rhs@(Imm RFloat _)) _ =
            "fmovi " <> display lhs <> ", " <> display rhs
        withoutState (ILMov _ lhs rhs) _ =
            "movi " <> display lhs <> ", " <> rhs
        withoutState (ICall _ func) _ =
            "call " <> func
        withoutState (ICallReg _ reg) _ =
            "callr " <> display reg
        withoutState (ILoad _ lhs@(Register RInt _) rhs offset) _ =
            "lw " <> display lhs <> ", " <> pack (show offset) <> "(" <> display rhs <> ")"
        withoutState (IStore _ lhs@(Register RInt _) rhs offset) _ =
            "sw " <> display lhs <> ", " <> pack (show offset) <> "(" <> display rhs <> ")"
        withoutState (ILoad _ lhs@(Register RFloat _) rhs offset) _ =
            "flw " <> display lhs <> ", " <> pack (show offset) <> "(" <> display rhs <> ")"
        withoutState (IStore _ lhs@(Register RFloat _) rhs offset) _ =
            "fsw " <> display lhs <> ", " <> pack (show offset) <> "(" <> display rhs <> ")"
        withoutState (IRawInst _ name ret iArgs fArgs) _ =
            name
                <> " "
                <> intercalate ", " (display ret : map display iArgs ++ map display fArgs)
        withoutState (IPhi _ dest choices) _ =
            "phi! " <> display dest <> ", " <> intercalate ", " (map (\(l, r) -> l <> ": " <> display r) $ toAscList choices)

instance (Display (InstStateTy ty)) => Display (Inst ty) where
    display = displayI 0

getIState :: Inst ty -> InstStateTy ty
getIState (ICompOp state _ _ _ _) = state
getIState (IIntOp state _ _ _ _) = state
getIState (IFOp state _ _ _ _) = state
getIState (IMov state _ _) = state
getIState (ILMov state _ _) = state
getIState (ICall state _) = state
getIState (ICallReg state _) = state
getIState (ILoad state _ _ _) = state
getIState (IStore state _ _ _) = state
getIState (IRawInst state _ _ _ _) = state
getIState (IPhi state _ _) = state

substIState ::
    (AllowPhi a ~ AllowPhi b) =>
    (InstStateTy a -> InstStateTy b) ->
    Inst a ->
    Inst b
substIState f (ICompOp state op dest src1 src2) = ICompOp (f state) op dest src1 src2
substIState f (IIntOp state op dest src1 src2) = IIntOp (f state) op dest src1 src2
substIState f (IFOp state op dest src1 src2) = IFOp (f state) op dest src1 src2
substIState f (IMov state dest src) = IMov (f state) dest src
substIState f (ILMov state dest src) = ILMov (f state) dest src
substIState f (ICall state label) = ICall (f state) label
substIState f (ICallReg state reg) = ICallReg (f state) reg
substIState f (ILoad state dest src offset) = ILoad (f state) dest src offset
substIState f (IStore state dest src offset) = IStore (f state) dest src offset
substIState f (IRawInst state inst retTy iArgs fArgs) = IRawInst (f state) inst retTy iArgs fArgs
substIState f (IPhi state dest choices) = IPhi (f state) dest choices

instance RegReplaceable (Inst ty) where
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
    mapReg substR (IPhi state dest choices) =
        IPhi state (substR dest) (M.map substR choices)
