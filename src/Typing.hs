{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Typing (
    TypeId,
    ITy,
    Ty,
    TypeKind (TUnit, TBool, TInt, TFloat, TFun, TTuple, TArray, TVar),
    weakenTy,
) where

import Data.Text (intercalate, pack)
import Display (Display (display))

data TypeNotResolved = TypeNotResolved
    deriving (Show, Eq, Ord)
data TypeResolved = TypeResolved
    deriving (Show, Eq, Ord)

type TypeId = Int

type ITy = TypeKind TypeNotResolved
type Ty = TypeKind TypeResolved

data TypeKind resolvedTy where
    TUnit :: TypeKind resolvedTy
    TBool :: TypeKind resolvedTy
    TInt :: TypeKind resolvedTy
    TFloat :: TypeKind resolvedTy
    TFun :: [TypeKind resolvedTy] -> TypeKind resolvedTy -> TypeKind resolvedTy
    TTuple :: [TypeKind resolvedTy] -> TypeKind resolvedTy
    TArray :: TypeKind resolvedTy -> TypeKind resolvedTy
    TVar :: TypeId -> TypeKind TypeNotResolved

deriving instance (Show a) => Show (TypeKind a)
deriving instance (Eq a) => Eq (TypeKind a)
deriving instance (Ord a) => Ord (TypeKind a)

instance Display (TypeKind a) where
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
