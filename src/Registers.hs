{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Registers (
    RegID,
    RegType (..),
    withRegType,
    RegVariant (..),
    RegVariant',
    VariantItem (..),
    updateVariant,
    (#$),
    (#!!),
    RegisterKind (..),
    Register (..),
    zeroReg,
    returnReg,
    retReg,
    heapReg,
    stackReg,
    argsReg,
    tempReg,
    savedReg,
    generalReg,
    RegOrImm (..),
    compareReg,
    compareRegOrImm,
) where

import Data.Text (pack)
import Display (Display (display))
import Typing (Ty, TypeKind (TFloat))

type RegID = Int

-- | Holds the type of a register.
data RegType ty where
    RInt :: RegType Int
    RFloat :: RegType Float

deriving instance Show (RegType ty)
deriving instance Eq (RegType ty)

withRegType :: Ty -> (forall a. RegType a -> b) -> b
withRegType TFloat f = f RFloat
withRegType _ f = f RInt

-- | Holds two objects - one is for integer registers and the other is for float registers.
data RegVariant f
    = RegVariant
    { iVariant :: f Int
    , fVariant :: f Float
    }

deriving instance (Show (f Int), Show (f Float)) => Show (RegVariant f)
deriving instance (Eq (f Int), Eq (f Float)) => Eq (RegVariant f)

newtype VariantItem b a
    = VariantItem {unwrap :: b}
    deriving (Eq)

instance (Show b) => Show (VariantItem b a) where
    show (VariantItem item) = show item

type RegVariant' a = RegVariant (VariantItem a)

selectVariant :: RegType a -> RegVariant f -> f a
selectVariant RInt (RegVariant i _) = i
selectVariant RFloat (RegVariant _ f) = f

infixl 9 #!!
(#!!) :: RegVariant f -> RegType a -> f a
(#!!) = flip selectVariant

updateVariant :: RegType a -> (f a -> f a) -> RegVariant f -> RegVariant f
updateVariant RInt func variant = variant{iVariant = func $ iVariant variant}
updateVariant RFloat func variant = variant{fVariant = func $ fVariant variant}

mapVariant :: (forall a. f a -> g a) -> RegVariant f -> RegVariant g
mapVariant f (RegVariant i f') = RegVariant (f i) (f f')

infixl 4 #$
(#$) :: (forall a. f a -> g a) -> RegVariant f -> RegVariant g
(#$) = mapVariant

instance (Semigroup (f Int), Semigroup (f Float)) => Semigroup (RegVariant f) where
    RegVariant i1 f1 <> RegVariant i2 f2 = RegVariant (i1 <> i2) (f1 <> f2)

instance (Monoid (f Int), Monoid (f Float)) => Monoid (RegVariant f) where
    mempty = RegVariant mempty mempty

data RegisterKind ty where
    ZeroReg :: RegisterKind ty
    ReturnReg :: RegisterKind Int
    RetReg :: RegisterKind ty
    HeapReg :: RegisterKind Int
    StackReg :: RegisterKind Int
    ArgsReg :: RegID -> RegisterKind ty
    TempReg :: RegID -> RegisterKind ty
    SavedReg :: RegID -> RegisterKind ty
    GeneralReg :: RegID -> RegisterKind ty
    -- | Used for marking it as a "don't care" register.
    DCReg :: RegisterKind ty

deriving instance Show (RegisterKind ty)
deriving instance Eq (RegisterKind ty)

data Register ty
    = Register
    { regType :: RegType ty
    , regKind :: RegisterKind ty
    }

deriving instance Show (Register ty)
deriving instance Eq (Register ty)

instance Display (Register a) where
    display (Register RInt ZeroReg) = "zero"
    display (Register RFloat ZeroReg) = "fzero"
    display (Register _ ReturnReg) = "ra"
    display (Register RInt RetReg) = "a0"
    display (Register RFloat RetReg) = "fa0"
    display (Register _ HeapReg) = "hp"
    display (Register _ StackReg) = "sp"
    display (Register RInt (ArgsReg idTy)) = "a" <> pack (show idTy)
    display (Register RFloat (ArgsReg idTy)) = "fa" <> pack (show idTy)
    display (Register RInt (TempReg idTy)) = "t" <> pack (show idTy)
    display (Register RFloat (TempReg idTy)) = "ft" <> pack (show idTy)
    display (Register RInt (SavedReg idTy)) = "s" <> pack (show idTy)
    display (Register RFloat (SavedReg idTy)) = "fs" <> pack (show idTy)
    display (Register RInt (GeneralReg idTy)) = "r" <> pack (show idTy)
    display (Register RFloat (GeneralReg idTy)) = "fr" <> pack (show idTy)
    display (Register RInt DCReg) = "zero" -- Don't care
    display (Register RFloat DCReg) = "fzero" -- Don't care

compareReg :: Register ty1 -> Register ty2 -> Bool
compareReg (Register RInt rKind1) (Register RInt rKind2) = rKind1 == rKind2
compareReg (Register RFloat rKind1) (Register RFloat rKind2) = rKind1 == rKind2
compareReg _ _ = False

zeroReg :: RegType ty -> Register ty
zeroReg rTy = Register rTy ZeroReg

returnReg :: Register Int
returnReg = Register RInt ReturnReg

retReg :: RegType ty -> Register ty
retReg rTy = Register rTy RetReg

heapReg :: Register Int
heapReg = Register RInt HeapReg

stackReg :: Register Int
stackReg = Register RInt StackReg

argsReg :: RegType ty -> RegID -> Register ty
argsReg rTy i = Register rTy (ArgsReg i)

tempReg :: RegType ty -> RegID -> Register ty
tempReg rTy i = Register rTy (TempReg i)

savedReg :: RegType ty -> RegID -> Register ty
savedReg rTy i = Register rTy (SavedReg i)

generalReg :: RegType ty -> RegID -> Register ty
generalReg rTy i = Register rTy (GeneralReg i)

data RegOrImm ty where
    Reg :: Register ty -> RegOrImm ty
    Imm :: RegType ty -> ty -> RegOrImm ty

instance Show (RegOrImm ty) where
    show (Reg reg) = show reg
    show (Imm RInt imm) = show imm
    show (Imm RFloat imm) = show imm

instance Eq (RegOrImm ty) where
    Reg reg1 == Reg reg2 = reg1 == reg2
    Imm RInt imm1 == Imm RInt imm2 = imm1 == imm2
    Imm RFloat imm1 == Imm RFloat imm2 = imm1 == imm2
    _ == _ = False

instance Display (RegOrImm Int) where
    display (Reg reg) = display reg
    display (Imm _ imm) = pack (show imm)

instance Display (RegOrImm Float) where
    display (Reg reg) = display reg
    display (Imm _ imm) = pack (show imm)

compareRegOrImm :: RegOrImm ty1 -> RegOrImm ty2 -> Bool
compareRegOrImm (Reg reg1) (Reg reg2) = compareReg reg1 reg2
compareRegOrImm (Imm RInt imm1) (Imm RInt imm2) = imm1 == imm2
compareRegOrImm (Imm RFloat imm1) (Imm RFloat imm2) = imm1 == imm2
compareRegOrImm _ _ = False
