{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module BackEnd.Spill (spill) where

import BackEnd.Analysis.IR (InOutSet (InOutSet), inOutRegisters)
import BackEnd.BackendEnv (BackendStateT, genTempReg)
import CodeBlock (BlockGraph (localVars), CodeBlock (blockInst, terminator), Terminator (TBranch), VirtualBlock, VirtualBlockGraph, visitBlock)
import Control.Monad.Except (MonadError (throwError))
import Data.Set (member)
import Error (CompilerError (UnexpectedError))
import IR (
    AbstInst,
    Inst (..),
    getIState,
 )
import Registers (
    RegID,
    RegType,
    compareReg,
    replaceReg,
    savedReg,
    stackReg,
    (#!!),
 )
import Syntax (dummyLoc)

-- | Replace the register with a local variable.
replaceRegWithMem :: (Monad m) => RegType a -> RegID -> Int -> AbstInst -> BackendStateT m [AbstInst]
replaceRegWithMem rTy reg local inst = do
    let (InOutSet inSet outSet) = inOutRegisters inst #!! rTy

    (prologue, inst', epilogue) <-
        case (reg `member` inSet, reg `member` outSet) of
            (True, True) ->
                throwError $ UnexpectedError "A register cannot be both in and out at the same time."
            (True, False) -> do
                -- If the input registers contain the register to be spilled, replace it with a new register.
                newReg <- genTempReg rTy
                let inst' = replaceReg (savedReg rTy reg) newReg inst
                pure ([ILoad (getIState inst) newReg stackReg local], inst', [])
            (False, True) -> do
                -- If the output registers contain the register to be spilled, replace it with a new register.
                newReg <- genTempReg rTy
                let inst' = replaceReg (savedReg rTy reg) newReg inst
                pure ([], inst', [IStore (getIState inst) newReg stackReg local])
            (False, False) -> pure ([], inst, [])

    pure $ prologue <> [inst'] <> epilogue

-- | Replace the register with a local variable in the block.
spillBlock :: (Monad m) => RegType a -> RegID -> Int -> VirtualBlock -> BackendStateT m VirtualBlock
spillBlock rTy reg local block = do
    -- Spill the register in each instruction.
    inst' <- concat <$> mapM (replaceRegWithMem rTy reg local) (blockInst block)

    -- The register may be used in the terminator.
    (prologue, term') <- case terminator block of
        term@(TBranch _ lhs rhs _ _) -> do
            let target = savedReg rTy reg
            if compareReg lhs target || compareReg rhs target
                then do
                    newReg <- genTempReg rTy
                    pure ([ILoad dummyLoc newReg stackReg local], replaceReg target newReg term)
                else do
                    pure ([], term)
        term -> pure ([], term)

    -- The "prologue" is for the terminator, so it should be placed right before that.
    pure $ block{blockInst = inst' <> prologue, terminator = term'}

-- | Store the value of the register to the stack and replace it with a new one.
spill :: (Monad m) => RegType a -> RegID -> VirtualBlockGraph -> BackendStateT m VirtualBlockGraph
spill rTy reg graph = do
    graph' <- visitBlock (spillBlock rTy reg (localVars graph)) graph
    pure $ graph'{localVars = localVars graph + 1}
