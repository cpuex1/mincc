{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Closure (getFunctions, ClosureEnv (ClosureEnv)) where

import Control.Monad (unless)
import Control.Monad.State (State, execState, get, gets, modify, runState)
import Syntax

newtype ClosureEnv = ClosureEnv
    { functions :: [Function]
    }

type ClosureState = State ClosureEnv

addFunction :: Function -> ClosureState ()
addFunction f = do
    modify $ \env -> env{functions = f : functions env}

findFunction :: Ident -> ClosureState (Maybe Function)
findFunction ident =
    gets (findFunction' ident . functions)
  where
    findFunction' _ [] = Nothing
    findFunction' ident' (f : fs)
        | funcName f == ident' = Just f
        | otherwise = findFunction' ident' fs

data GlobalArgsEnv = GlobalArgsEnv
    { eGlobalArgs :: [Ident]
    , eBoundedArgs :: [Ident]
    }

extractGlobal :: Ident -> State GlobalArgsEnv ()
extractGlobal ident = do
    env <- get
    unless (ident `elem` eBoundedArgs env) $
        unless (ident `elem` eGlobalArgs env) $
            modify $
                \env' -> env'{eGlobalArgs = ident : eGlobalArgs env}

addBoundedArg :: Ident -> State GlobalArgsEnv ()
addBoundedArg ident = modify $ \env -> env{eBoundedArgs = ident : eBoundedArgs env}

getGlobalArgs :: KExpr -> State GlobalArgsEnv ()
getGlobalArgs (Const _ _) = pure ()
getGlobalArgs (Unary _ _ operand) = extractGlobal operand
getGlobalArgs (Binary _ _ operand1 operand2) = do
    extractGlobal operand1
    extractGlobal operand2
getGlobalArgs (If _ cond thenExpr elseExpr) = do
    extractGlobal cond
    getGlobalArgs thenExpr
    getGlobalArgs elseExpr
getGlobalArgs (Let _ PUnit expr body) = do
    getGlobalArgs expr
    getGlobalArgs body
getGlobalArgs (Let _ (PVar v) expr body) = do
    -- Since the identifiers are unique, we don't have to care about shadowing.
    addBoundedArg v
    getGlobalArgs expr
    getGlobalArgs body
getGlobalArgs (Let _ (PTuple vs) expr body) = do
    mapM_ addBoundedArg vs
    getGlobalArgs expr
    getGlobalArgs body
getGlobalArgs (Let _ (PRec func args) expr body) = do
    addBoundedArg func
    mapM_ addBoundedArg args
    getGlobalArgs expr
    getGlobalArgs body
getGlobalArgs (Var _ v) = do
    extractGlobal v
getGlobalArgs (Tuple _ values) = do
    mapM_ extractGlobal values
getGlobalArgs (ArrayCreate _ size value) = do
    extractGlobal size
    extractGlobal value
getGlobalArgs (Get _ array index) = do
    extractGlobal array
    extractGlobal index
getGlobalArgs (Put _ array index value) = do
    extractGlobal array
    extractGlobal index
    extractGlobal value
getGlobalArgs (App _ func args) = do
    extractGlobal func
    mapM_ extractGlobal args

getFunctions :: KExpr -> [Function]
getFunctions expr =
    Function (getExprState expr') True (ExternalIdent "__entry") [] [] expr' : functions funcList
    where
        (expr', funcList) = runState (extractFunctions expr) (ClosureEnv [])

extractFunctions :: KExpr -> ClosureState ClosureExpr
extractFunctions (Let state (PRec func args) expr body) = do
    expr' <- extractFunctions expr
    addFunction (Function state False func globals args expr')
    body' <- extractFunctions body
    pure $ Let state (PVar func) (MakeClosure state func globals) body'
  where
    globals = eGlobalArgs $ execState (getGlobalArgs expr) (GlobalArgsEnv [] (func : args))
extractFunctions (App state func args) = do
    func' <- findFunction func
    case (func', func) of
        (Just f, _) -> do
            if isDirect f
                then do
                    pure $ DirectApp state func args
                else do
                    pure $ ClosureApp state func args
        (Nothing, ExternalIdent _) -> do
            -- Calling an external function.
            pure $ DirectApp state func args
        (Nothing, _) -> do
            -- Calling a closure.
            pure $ ClosureApp state func args
extractFunctions (Const state lit) =
    pure $ Const state lit
extractFunctions (Unary state op operand) =
    pure $ Unary state op operand
extractFunctions (Binary state op operand1 operand2) =
    pure $ Binary state op operand1 operand2
extractFunctions (If state cond thenExpr elseExpr) = do
    thenExpr' <- extractFunctions thenExpr
    elseExpr' <- extractFunctions elseExpr
    pure $ If state cond thenExpr' elseExpr'
extractFunctions (Let state PUnit expr body) = do
    expr' <- extractFunctions expr
    body' <- extractFunctions body
    pure $ Let state PUnit expr' body'
extractFunctions (Let state (PVar v) expr body) = do
    expr' <- extractFunctions expr
    body' <- extractFunctions body
    pure $ Let state (PVar v) expr' body'
extractFunctions (Let state (PTuple vs) expr body) = do
    expr' <- extractFunctions expr
    body' <- extractFunctions body
    pure $ Let state (PTuple vs) expr' body'
extractFunctions (Var state ident) = pure $ Var state ident
extractFunctions (Tuple state values) =
    pure $ Tuple state values
extractFunctions (ArrayCreate state size value) =
    pure $ ArrayCreate state size value
extractFunctions (Get state array index) =
    pure $ Get state array index
extractFunctions (Put state array index value) =
    pure $ Put state array index value
