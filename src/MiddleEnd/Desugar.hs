{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module MiddleEnd.Desugar (expandArrayCreate) where

import Control.Monad (foldM)
import Control.Monad.State (MonadTrans (lift), StateT (runStateT), modify)
import Display (Display (display))
import FrontEnd.Flatten (flattenExpr)
import MiddleEnd.Analysis.Constant (registerConstants)
import MiddleEnd.Analysis.Identifier (IdentEnvT, IdentProp (IdentProp), asConstant, genNewVar, registerProp)
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
    TState (
        TState,
        getType
    ),
    dummyLoc,
    getExprState,
 )
import Typing (PTy, TypeBase (..))

-- Use List instead of Set because ordering is not defined for `KExpr -> IdentEnvT m KExpr`.
type ArrayCreateStateT m = StateT [KExpr -> IdentEnvT m KExpr] (IdentEnvT m)

constSizeLimit :: Int
constSizeLimit = 256

generateInitArrayFunc :: (Monad m) => Ident -> PTy -> KExpr -> IdentEnvT m KExpr
generateInitArrayFunc uniqueId valTy body = do
    array <- genNewVar (TArray valTy)
    idx <- genNewVar TInt
    size <- genNewVar TInt
    value <- genNewVar valTy
    inc <- genNewVar TInt
    incResult <- genNewVar TInt

    -- Registers variables.
    registerProp uniqueId (IdentProp func_ty Nothing False Nothing)

    let func_body =
            If
                (TState TUnit dummyLoc)
                (CComp Lt idx size)
                ( Let
                    (TState TUnit dummyLoc)
                    PUnit
                    (Put (TState TUnit dummyLoc) array idx value)
                    ( Let
                        (TState TUnit dummyLoc)
                        (PVar inc)
                        (Const (TState TInt dummyLoc) (LInt 1))
                        ( Let
                            (TState TUnit dummyLoc)
                            (PVar incResult)
                            (Binary (TState TInt dummyLoc) (IntOp Add) idx inc)
                            ( App
                                (TState TUnit dummyLoc)
                                uniqueId
                                [array, incResult, size, value]
                            )
                        )
                    )
                )
                (Const (TState TUnit dummyLoc) LUnit)
    let bodyTy = getType $ getExprState body
    pure $ Let (TState bodyTy dummyLoc) (PRec uniqueId [array, idx, size, value]) func_body body
  where
    func_ty = TFun [TArray valTy, TInt, TInt, valTy] TUnit

expandArrayCreate :: (Monad m) => KExpr -> IdentEnvT m KExpr
expandArrayCreate expr = do
    registerConstants expr
    (expr', funcGenerators) <- runStateT (expandArrayCreate' expr) []
    flattenExpr <$> foldM (\e gen -> gen e) expr' funcGenerators
  where
    expandArrayCreate' :: (Monad m) => KExpr -> ArrayCreateStateT m KExpr
    expandArrayCreate' (Let state1 (PVar v) (ArrayCreate (TState (TArray valTy) loc) size val) body) = do
        body' <- expandArrayCreate' body

        size' <- lift $ asConstant size
        optimized <- case size' of
            Just (LInt constSize) -> do
                if constSize < constSizeLimit
                    then do
                        assignment <-
                            lift $
                                foldM
                                    ( \child idx -> do
                                        idxVar <- genNewVar TInt
                                        pure $
                                            Let
                                                state1
                                                (PVar idxVar)
                                                (Const (TState TInt loc) (LInt idx))
                                                ( Let
                                                    state1
                                                    PUnit
                                                    (Put (TState TUnit loc) v idxVar val)
                                                    child
                                                )
                                    )
                                    body'
                                    [0 .. constSize - 1]
                        pure $
                            Just $
                                Let
                                    state1
                                    (PVar v)
                                    (ArrayCreate (TState (TArray valTy) loc) size val)
                                    assignment
                    else
                        pure Nothing
            _ -> pure Nothing

        case optimized of
            Just expr'' -> pure expr''
            Nothing -> do
                let funcGen = generateInitArrayFunc uniqueId valTy
                modify $ \ctx -> funcGen : ctx
                zero <- lift $ genNewVar TInt
                pure $
                    Let
                        state1
                        (PVar v)
                        (ArrayCreate (TState (TArray valTy) loc) size val)
                        ( Let
                            state1
                            (PVar zero)
                            (Const (TState TInt loc) (LInt 0))
                            ( Let
                                state1
                                PUnit
                                (App (TState TUnit loc) uniqueId [v, zero, size, val])
                                body'
                            )
                        )
      where
        uniqueId = UserDefined dummyLoc $ "Array.create" <> display v
    expandArrayCreate' (Let state pat expr' body) = do
        expr'' <- expandArrayCreate' expr'
        body' <- expandArrayCreate' body
        pure $ Let state pat expr'' body'
    expandArrayCreate' (If state cond then' else') = do
        then'' <- expandArrayCreate' then'
        else'' <- expandArrayCreate' else'
        pure $ If state cond then'' else''
    expandArrayCreate' (Loop state args values body) = do
        body' <- expandArrayCreate' body
        pure $ Loop state args values body'
    expandArrayCreate' (ArrayCreate state size val) = do
        temp <- lift $ genNewVar (getType state)
        expandArrayCreate' (Let state (PVar temp) (ArrayCreate state size val) (Var state temp))
    expandArrayCreate' expr' = pure expr'
