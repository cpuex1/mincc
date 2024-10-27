{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Typing (ITy, Ty, TypeKind(TUnit, TBool, TInt, TFloat, TFun, TTuple, TArray, TVar)) where

data TypeNotResolved = TypeNotResolved
    deriving (Show, Eq)
data TypeResolved = TypeResolved
    deriving (Show, Eq)

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

deriving instance Show a => Show (TypeKind a)
deriving instance Eq a => Eq (TypeKind a)
