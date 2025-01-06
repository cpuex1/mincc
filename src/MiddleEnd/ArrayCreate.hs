{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module MiddleEnd.ArrayCreate (expandArrayCreate) where

import Data.Text (Text)
import Display (Display (display))
import FrontEnd.Flatten (flattenExpr)
import MiddleEnd.Analysis.Identifier (IdentEnvT, IdentProp (IdentProp), genNewVar, registerProp)
import Syntax (
    BinaryOp (IntOp),
    Cond (CComp),
    Expr (..),
    Ident (UserDefined),
    IntBinOp (Add),
    KExpr,
    Literal (LInt, LUnit),
    Pattern (PRec, PUnit, PVar),
    RelationBinOp (Lt),
    TypedState (
        TypedState,
        getType
    ),
    dummyLoc,
    getExprState,
 )
import Typing (Ty, TypeKind (..))

generateInitArrayFunc :: (Monad m) => Text -> Ty -> (Ident -> IdentEnvT m KExpr) -> IdentEnvT m KExpr
generateInitArrayFunc uniqueId valTy bodyGen = do
    body <- bodyGen init_array_func

    array <- genNewVar (TArray valTy)
    idx <- genNewVar TInt
    size <- genNewVar TInt
    value <- genNewVar valTy
    inc <- genNewVar TInt
    incResult <- genNewVar TInt

    -- Registers variables.
    registerProp init_array_func (IdentProp func_ty Nothing False)

    let func_body =
            If
                (TypedState TUnit dummyLoc)
                (CComp Lt idx size)
                ( Let
                    (TypedState TUnit dummyLoc)
                    PUnit
                    (Put (TypedState TUnit dummyLoc) array idx value)
                    ( Let
                        (TypedState TUnit dummyLoc)
                        (PVar inc)
                        (Const (TypedState TInt dummyLoc) (LInt 1))
                        ( Let
                            (TypedState TUnit dummyLoc)
                            (PVar incResult)
                            (Binary (TypedState TInt dummyLoc) (IntOp Add) idx inc)
                            ( App
                                (TypedState TUnit dummyLoc)
                                init_array_func
                                [array, incResult, size, value]
                            )
                        )
                    )
                )
                (Const (TypedState TUnit dummyLoc) LUnit)
    let bodyTy = getType $ getExprState body
    pure $ Let (TypedState bodyTy dummyLoc) (PRec init_array_func [array, idx, size, value]) func_body body
  where
    init_array_func = UserDefined dummyLoc $ "Array.create" <> uniqueId
    func_ty = TFun [TArray valTy, TInt, TInt, valTy] TUnit

expandArrayCreate :: (Monad m) => KExpr -> IdentEnvT m KExpr
expandArrayCreate expr = flattenExpr <$> expandArrayCreate' expr
  where
    expandArrayCreate' :: (Monad m) => KExpr -> IdentEnvT m KExpr
    expandArrayCreate' (Let state1 (PVar v) (ArrayCreate (TypedState (TArray valTy) loc) size val) body) = do
        body' <- expandArrayCreate' body
        generateInitArrayFunc
            (display v)
            valTy
            ( \func -> do
                zero <- genNewVar TInt
                pure $
                    Let
                        state1
                        (PVar v)
                        (ArrayCreate (TypedState (TArray valTy) loc) size val)
                        ( Let
                            state1
                            (PVar zero)
                            (Const (TypedState TInt loc) (LInt 0))
                            ( Let
                                state1
                                PUnit
                                (App (TypedState TUnit loc) func [v, zero, size, val])
                                body'
                            )
                        )
            )
    expandArrayCreate' (Let state pat expr' body) = do
        expr'' <- expandArrayCreate' expr'
        body' <- expandArrayCreate' body
        pure $ Let state pat expr'' body'
    expandArrayCreate' (If state cond then' else') = do
        then'' <- expandArrayCreate' then'
        else'' <- expandArrayCreate' else'
        pure $ If state cond then'' else''
    expandArrayCreate' (ArrayCreate state size val) = do
        temp <- genNewVar (getType state)
        expandArrayCreate' (Let state (PVar temp) (ArrayCreate state size val) (Var state temp))
    expandArrayCreate' expr' = pure expr'
