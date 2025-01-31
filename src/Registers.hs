{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Registers (
    RegType (..),
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
) where

-- | Holds the type of a register.
data RegType ty where
    RInt :: RegType Int
    RFloat :: RegType Float

deriving instance (Show ty) => Show (RegType ty)
deriving instance (Eq ty) => Eq (RegType ty)

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

data RegisterKind idTy ty where
    ZeroReg :: RegisterKind idTy ty
    ReturnReg :: RegisterKind idTy Int
    RetReg :: RegisterKind idTy ty
    HeapReg :: RegisterKind idTy Int
    StackReg :: RegisterKind idTy Int
    ArgsReg :: idTy -> RegisterKind idTy ty
    TempReg :: idTy -> RegisterKind idTy ty
    SavedReg :: idTy -> RegisterKind idTy ty
    GeneralReg :: idTy -> RegisterKind idTy ty
    -- | Used for marking it as a "don't care" register.
    DCReg :: RegisterKind idTy ty

deriving instance (Show idTy, Show ty) => Show (RegisterKind idTy ty)
deriving instance (Eq idTy, Eq ty) => Eq (RegisterKind idTy ty)

data Register idTy ty
    = Register
    { regType :: RegType ty
    , regKind :: RegisterKind idTy ty
    }
    deriving (Show, Eq)

zeroReg :: RegType ty -> Register idTy ty
zeroReg rTy = Register rTy ZeroReg

returnReg :: Register idTy Int
returnReg = Register RInt ReturnReg

retReg :: RegType ty -> Register idTy ty
retReg rTy = Register rTy RetReg

heapReg :: Register idTy Int
heapReg = Register RInt HeapReg

stackReg :: Register idTy Int
stackReg = Register RInt StackReg

argsReg :: RegType ty -> idTy -> Register idTy ty
argsReg rTy i = Register rTy (ArgsReg i)

tempReg :: RegType ty -> idTy -> Register idTy ty
tempReg rTy i = Register rTy (TempReg i)

savedReg :: RegType ty -> idTy -> Register idTy ty
savedReg rTy i = Register rTy (SavedReg i)

generalReg :: RegType ty -> idTy -> Register idTy ty
generalReg rTy i = Register rTy (GeneralReg i)

data RegOrImm idTy ty where
    Reg :: Register idTy ty -> RegOrImm idTy ty
    Imm :: ty -> RegOrImm idTy ty

deriving instance (Show idTy, Show ty) => Show (RegOrImm idTy ty)
deriving instance (Eq idTy, Eq ty) => Eq (RegOrImm idTy ty)
