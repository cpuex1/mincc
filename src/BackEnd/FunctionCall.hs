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
    RegType (RFloat, RInt),
    RegVariant,
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

genPrologueAndEpilogue :: RegType a -> RegVariant Liveness -> FunctionCallState (Seq LivenessInst, Seq LivenessInst)
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
saveRegBeyondCall rTy (IRichCall (LivenessLoc loc l) label iArgs fArgs) = do
    (prologue, epilogue) <- genPrologueAndEpilogue rTy l
    pure $ prologue <> singleton (IRichCall (LivenessLoc loc l) label iArgs fArgs) <> epilogue
saveRegBeyondCall rTy (IClosureCall (LivenessLoc loc l) cl iArgs fArgs) = do
    (prologue, epilogue) <- genPrologueAndEpilogue rTy l
    pure $ prologue <> singleton (IClosureCall (LivenessLoc loc l) cl iArgs fArgs) <> epilogue
saveRegBeyondCall rTy (IBranch state op left right thenBlock elseBlock) = do
    thenBlock' <- toList <$> saveRegBeyondCallAll thenBlock
    elseBlock' <- toList <$> saveRegBeyondCallAll elseBlock
    pure $
        singleton $
            IBranch
                state
                op
                left
                right
                thenBlock'
                elseBlock'
  where
    saveRegBeyondCallAll :: [LivenessInst] -> FunctionCallState (Seq LivenessInst)
    saveRegBeyondCallAll = foldM (\already block -> (already <>) <$> saveRegBeyondCall rTy block) empty
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
