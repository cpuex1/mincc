{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Typing (
    TypeId,
    ITy,
    Ty,
    PTy,
    TypeBase (..),
    weakenTy,
    removeBoolTy,
) where

import Data.Text (intercalate, pack)
import Display (Display (display))

type TypeId = Int

class TypeKind ty where
    type TypeResolved ty :: Bool
    type AllowBoolType ty :: Bool

data TyKind

instance TypeKind TyKind where
    type TypeResolved TyKind = True
    type AllowBoolType TyKind = True

-- | The type.
type Ty = TypeBase TyKind

data PTyKind

instance TypeKind PTyKind where
    type TypeResolved PTyKind = True
    type AllowBoolType PTyKind = False

-- | The primitive type.
type PTy = TypeBase PTyKind

data ITyKind

instance TypeKind ITyKind where
    type TypeResolved ITyKind = False
    type AllowBoolType ITyKind = True

-- | The intermediate type.
type ITy = TypeBase ITyKind

data TypeBase ty where
    TUnit :: TypeBase ty
    TBool ::
        (AllowBoolType ty ~ True) =>
        TypeBase ty
    TInt :: TypeBase ty
    TFloat :: TypeBase ty
    TFun :: [TypeBase ty] -> TypeBase ty -> TypeBase ty
    TTuple :: [TypeBase ty] -> TypeBase ty
    TArray :: TypeBase ty -> TypeBase ty
    TVar ::
        (TypeResolved ty ~ False) =>
        TypeId ->
        TypeBase ty

deriving instance Show (TypeBase a)
deriving instance Eq (TypeBase a)
deriving instance Ord (TypeBase a)

instance Display (TypeBase a) where
    display TUnit = "unit"
    display TBool = "bool"
    display TInt = "int"
    display TFloat = "float"
    display (TFun args ret) =
        "(" <> intercalate ", " (Prelude.map display args) <> ") -> " <> display ret
    display (TTuple values) =
        "(" <> intercalate " * " (Prelude.map display values) <> ")"
    display (TArray value) =
        display value <> " array"
    display (TVar tId) = "__t" <> pack (show tId)

weakenTy :: Ty -> ITy
weakenTy TUnit = TUnit
weakenTy TBool = TBool
weakenTy TInt = TInt
weakenTy TFloat = TFloat
weakenTy (TFun args ret) = TFun (map weakenTy args) (weakenTy ret)
weakenTy (TTuple vals) = TTuple (map weakenTy vals)
weakenTy (TArray array) = TArray (weakenTy array)

-- | Regards bool as int.
removeBoolTy :: Ty -> PTy
removeBoolTy TBool = TInt
removeBoolTy TUnit = TUnit
removeBoolTy TInt = TInt
removeBoolTy TFloat = TFloat
removeBoolTy (TFun args ret) = TFun (map removeBoolTy args) (removeBoolTy ret)
removeBoolTy (TTuple vals) = TTuple (map removeBoolTy vals)
removeBoolTy (TArray array) = TArray (removeBoolTy array)
