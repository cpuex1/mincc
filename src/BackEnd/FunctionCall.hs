{-# LANGUAGE GADTs #-}

module BackEnd.FunctionCall (
    saveRegisters,
) where

import BackEnd.Liveness (Liveness (alive), LivenessInst, LivenessLoc (LivenessLoc, livenessLoc), liveness)
import Control.Monad (foldM)
import Control.Monad.State (MonadState (get, put), State, runState)
import Data.Foldable (toList)
import Data.Sequence (Seq, empty, singleton)
import IR (
    AbstCodeBlock,
    HCodeBlock (hInst, localVars),
    Inst (..),
    substIState,
 )
import Registers (
    RegMultiple,
    RegType (RFloat, RInt),
    savedReg,
    stackReg,
    (#!!),
 )
import Syntax (dummyLoc)

type FunctionCallState = State Int

genLocalVar :: FunctionCallState Int
genLocalVar = do
    i <- get
    put $ i + 1
    return i

dummyLivenessLoc :: LivenessLoc
dummyLivenessLoc = LivenessLoc dummyLoc mempty

genPrologueAndEpilogue :: RegType a -> RegMultiple Liveness -> FunctionCallState (Seq LivenessInst, Seq LivenessInst)
genPrologueAndEpilogue rTy l = do
    foldM
        ( \(prologue, epilogue) reg -> do
            local <- genLocalVar
            pure
                ( prologue
                    <> singleton
                        (IStore dummyLivenessLoc (savedReg rTy reg) stackReg local)
                , epilogue
                    <> singleton
                        (ILoad dummyLivenessLoc (savedReg rTy reg) stackReg local)
                )
        )
        (empty, empty)
        toBeSaved
  where
    toBeSaved = alive $ l #!! rTy

-- | Saves registers on the stack before a function call and restores them after the call.
saveRegBeyondCall :: RegType a -> LivenessInst -> FunctionCallState (Seq LivenessInst)
saveRegBeyondCall rTy (ICall (LivenessLoc loc l) label) = do
    (prologue, epilogue) <- genPrologueAndEpilogue rTy l
    pure $ prologue <> singleton (ICall (LivenessLoc loc l) label) <> epilogue
saveRegBeyondCall rTy (ICallReg (LivenessLoc loc l) cl) = do
    (prologue, epilogue) <- genPrologueAndEpilogue rTy l
    pure $ prologue <> singleton (ICallReg (LivenessLoc loc l) cl) <> epilogue
saveRegBeyondCall _ i = pure $ singleton i

-- | Saves registers on the stack before a function call and restores them after the call.
saveRegisters :: AbstCodeBlock -> AbstCodeBlock
saveRegisters block =
    block{localVars = locals, hInst = inst}
  where
    instWithLiveness = liveness $ hInst block
    (instList, locals) =
        runState
            ( do
                inst' <- foldl (<>) empty <$> mapM (saveRegBeyondCall RInt) instWithLiveness
                foldl (<>) empty <$> mapM (saveRegBeyondCall RFloat) inst'
            )
            $ localVars block
    inst = map (substIState livenessLoc) $ toList instList
