{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Registers (
    eachRegType,
    RegID,
    RegType (..),
    withRegType,
    RegVariant,
    RegMultiple,
    RegTuple (..),
    RegList,
    buildRT,
    buildRTM,
    updateRT,
    (#$),
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
    RegMapping (..),
    applyMapping,
    dcReg,
    RegReplaceable (..),
    weakSubstReg,
    substReg,
    coverImm,
    replaceReg,
    applyRegMapping,
) where

import Data.Map (Map, fromList, lookup, toList)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Display (Display (display))
import Typing (PTy, TypeBase (..))
import Prelude hiding (lookup)

type RegID = Int

-- | Holds the type of a register.
data RegType ty where
    RInt :: RegType Int
    RFloat :: RegType Float

deriving instance Show (RegType ty)
deriving instance Eq (RegType ty)
deriving instance Ord (RegType ty)

instance Display (RegType a) where
    display RInt = "int"
    display RFloat = "float"

-- | Execute a function with a register type corresponding to the type given.
withRegType :: PTy -> (forall a. Maybe (RegType a) -> b) -> b
withRegType TUnit f = f Nothing
withRegType TFloat f = f (Just RFloat)
withRegType _ f = f (Just RInt)

-- | Execute a function for each register type.
eachRegType :: (Monad m) => (forall a. RegType a -> m ()) -> m ()
eachRegType f = f RInt >> f RFloat

-- | Holds two objects - one is for integer registers and the other is for float registers.
class RegTuple f where
    type RegTupleMap f rTy

    infixl 9 #!!
    (#!!) :: f -> RegType rTy -> RegTupleMap f rTy
    createRT :: RegTupleMap f Int -> RegTupleMap f Float -> f

buildRT :: (RegTuple f) => (forall rTy. RegType rTy -> RegTupleMap f rTy) -> f
buildRT f = createRT (f RInt) (f RFloat)

buildRTM :: (Monad m, RegTuple f) => (forall rTy. RegType rTy -> m (RegTupleMap f rTy)) -> m f
buildRTM f = do
    i' <- f RInt
    f' <- f RFloat
    pure $ createRT i' f'

updateRT :: (RegTuple f) => RegType rTy -> (RegTupleMap f rTy -> RegTupleMap f rTy) -> f -> f
updateRT RInt func rt = createRT (func $ rt #!! RInt) (rt #!! RFloat)
updateRT RFloat func rt = createRT (rt #!! RInt) (func $ rt #!! RFloat)

infixl 4 #$
(#$) :: (RegTuple f, RegTuple g) => (forall rTy. RegType rTy -> RegTupleMap f rTy -> RegTupleMap g rTy) -> f -> g
f #$ rt = buildRT $ \regTy -> f regTy $ rt #!! regTy

infixl 6 #<>
(#<>) :: (RegTuple f, Semigroup (RegTupleMap f Int), Semigroup (RegTupleMap f Float)) => f -> f -> f
a #<> b = createRT (a #!! RInt <> b #!! RInt) (a #!! RFloat <> b #!! RFloat)

memptyRT :: (RegTuple f, Monoid (RegTupleMap f Int), Monoid (RegTupleMap f Float)) => f
memptyRT = createRT mempty mempty

-- | Holds the same type of objects for both integer and float registers.
data RegMultiple a
    = RegMultiple a a

deriving instance (Show a) => Show (RegMultiple a)
deriving instance (Eq a) => Eq (RegMultiple a)

instance (RegTuple (RegMultiple a)) where
    type RegTupleMap (RegMultiple a) rTy = a

    RegMultiple i _ #!! RInt = i
    RegMultiple _ f #!! RFloat = f

    createRT = RegMultiple

instance (Semigroup a) => Semigroup (RegMultiple a) where
    a <> b = a #<> b

instance (Monoid a) => Monoid (RegMultiple a) where
    mempty = memptyRT

instance Functor RegMultiple where
    fmap f (RegMultiple i f') = RegMultiple (f i) (f f')

instance Applicative RegMultiple where
    pure a = RegMultiple a a
    RegMultiple i f <*> RegMultiple i' f' = RegMultiple (i i') (f f')

-- | Holds two objects - one is for integer registers and the other is for float registers.
data RegVariant f
    = RegVariant (f Int) (f Float)

deriving instance (Show (f Int), Show (f Float)) => Show (RegVariant f)
deriving instance (Eq (f Int), Eq (f Float)) => Eq (RegVariant f)

instance RegTuple (RegVariant f) where
    type RegTupleMap (RegVariant f) rTy = f rTy

    RegVariant i _ #!! RInt = i
    RegVariant _ f #!! RFloat = f

    createRT = RegVariant

instance (Semigroup (f Int), Semigroup (f Float)) => Semigroup (RegVariant f) where
    a <> b = a #<> b

instance (Monoid (f Int), Monoid (f Float)) => Monoid (RegVariant f) where
    mempty = memptyRT

-- | A list of registers.
data RegList = RegList
    { iRegList :: [Register Int]
    , fRegList :: [Register Float]
    }
    deriving (Show, Eq)

instance RegTuple RegList where
    type RegTupleMap RegList rTy = [Register rTy]

    RegList i _ #!! RInt = i
    RegList _ f #!! RFloat = f

    createRT = RegList

instance Semigroup RegList where
    a <> b = a #<> b

instance Monoid RegList where
    mempty = memptyRT

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
deriving instance Ord (RegisterKind ty)

data Register ty
    = Register
    { regType :: RegType ty
    , regKind :: RegisterKind ty
    }

deriving instance Show (Register ty)
deriving instance Eq (Register ty)
deriving instance Ord (Register ty)

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
    display (Register RInt DCReg) = "dc" -- Don't care
    display (Register RFloat DCReg) = "fdc" -- Don't care

compareReg :: Register ty1 -> Register ty2 -> Bool
compareReg (Register RInt rKind1) (Register RInt rKind2) = rKind1 == rKind2
compareReg (Register RFloat rKind1) (Register RFloat rKind2) = rKind1 == rKind2
compareReg _ _ = False

weakSubstReg :: Register ty -> Register ty -> Register ty -> Register ty
weakSubstReg beforeReg afterReg victim =
    if victim == beforeReg then afterReg else victim

substReg :: Register ty1 -> Register ty1 -> Register ty2 -> Register ty2
substReg (Register RInt before) afterReg (Register RInt victim) =
    weakSubstReg (Register RInt before) afterReg (Register RInt victim)
substReg (Register RFloat before) afterReg (Register RFloat victim) =
    weakSubstReg (Register RFloat before) afterReg (Register RFloat victim)
substReg _ _ victim = victim

coverImm :: (Register ty -> Register ty) -> (RegOrImm ty -> RegOrImm ty)
coverImm _ (Imm rTy imm) = Imm rTy imm
coverImm substR (Reg reg) = Reg $ substR reg

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

dcReg :: RegType ty -> Register ty
dcReg rTy = Register rTy DCReg

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

{- | Represents a mapping from registers to registers.
This mapping can be partial.
-}
newtype RegMapping
    = RegMapping
    { regMap :: Map RegID RegID
    }
    deriving (Show, Eq)

instance Semigroup RegMapping where
    RegMapping a <> RegMapping b =
        -- It behaves like a composition of functions.
        RegMapping (a <> mappedB)
      where
        mappedB = fromList $ map (\(from, to) -> (from, fromMaybe to $ lookup to a)) $ toList b

instance Monoid RegMapping where
    mempty = RegMapping mempty

-- | Apply the register mapping to a register.
applyMapping :: RegType a -> RegMultiple RegMapping -> RegID -> RegID
applyMapping rTy mapping reg =
    fromMaybe reg $ lookup reg $ regMap (mapping #!! rTy)

-- | A class for replacing registers in a data structure.
class RegReplaceable a where
    -- | Map registers in a data structure.
    mapReg :: (forall regTy1. Register regTy1 -> Register regTy1) -> a -> a

-- | Replace a register with another register.
replaceReg :: (RegReplaceable a) => Register regTy -> Register regTy -> a -> a
replaceReg before after = mapReg (substReg before after)

-- | Apply a register mapping to a data structure.
applyRegMapping :: (RegReplaceable a) => RegMultiple RegMapping -> a -> a
applyRegMapping mapping =
    mapReg
        ( \case
            Register rTy (SavedReg regID) -> Register rTy (SavedReg $ applyMapping rTy mapping regID)
            reg -> reg
        )
