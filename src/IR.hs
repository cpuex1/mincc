{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module IR (
    RegID,
    InstLabel,
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
    mapReg,
    replaceReg,
) where

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
coverImm _ (Imm imm) = Imm imm
coverImm substR (Reg reg) = Reg $ substR reg

mapReg ::
    (forall regTy1. Register idTy regTy1 -> Register idTy regTy1) ->
    Inst stateTy idTy branchTy ->
    Inst stateTy idTy branchTy
mapReg substR (ICompOp state op dest left right) =
    ICompOp state op (substR dest) (substR left) (coverImm substR right)
mapReg substR (IFCompOp state op dest left right) =
    IFCompOp state op (substR dest) (substR left) (substR right)
mapReg substR (IIntOp state op dest left right) =
    IIntOp state op (substR dest) (substR left) (coverImm substR right)
mapReg substR (IFOp state op dest left right) =
    IFOp state op (substR dest) (substR left) (substR right)
mapReg substR (IMov state dest src) =
    IMov state (substR dest) (coverImm substR src)
mapReg substR (IFMov state dest src) =
    IFMov state (substR dest) (coverImm substR src)
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
mapReg substR (IFLoad state dest src offset) =
    IFLoad state (substR dest) (substR src) offset
mapReg substR (IStore state dest src offset) =
    IStore state (substR dest) (substR src) offset
mapReg substR (IFStore state dest src offset) =
    IFStore state (substR dest) (substR src) offset
mapReg substR (IRawInst state inst retTy iArgs fArgs) =
    case retTy of
        RIRUnit -> IRawInst state inst retTy (map (coverImm substR) iArgs) (map substR fArgs)
        RIRInt reg -> IRawInst state inst (RIRInt (substR reg)) (map (coverImm substR) iArgs) (map substR fArgs)
        RIRFloat reg -> IRawInst state inst (RIRFloat (substR reg)) (map (coverImm substR) iArgs) (map substR fArgs)
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
    (Eq idTy) =>
    Register idTy regTy ->
    Register idTy regTy ->
    Inst stateTy idTy branchTy ->
    Inst stateTy idTy branchTy
replaceReg before after = mapReg (substReg before after)
