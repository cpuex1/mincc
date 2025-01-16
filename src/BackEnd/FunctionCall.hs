{-# LANGUAGE GADTs #-}

module BackEnd.FunctionCall (
    saveRegisters,
) where

import BackEnd.Liveness (LivenessLoc (LivenessLoc, livenessLoc), LivenessState (LivenessState), liveness)
import Control.Monad (foldM)
import Control.Monad.State (MonadState (get, put), State, runState)
import Data.Foldable (toList)
import Data.Sequence (Seq, empty, singleton)
import Data.Set (Set)
import qualified Data.Set as S
import IR (
    AllowBranch,
    Inst (
        IBranch,
        IClosureCall,
        IFLoad,
        IFStore,
        ILoad,
        IRichCall,
        IStore
    ),
    IntermediateCodeBlock (getICBInst, localVars),
    RegID,
    substIState,
 )
import Registers (RegType (RFloat, RInt), savedReg, stackReg)
import Syntax (Loc, dummyLoc)

type FunctionCallState = State Int

genLocalVar :: FunctionCallState Int
genLocalVar = do
    i <- get
    put $ i + 1
    return i

selectByType :: RegType a -> LivenessState -> Set RegID
selectByType RInt (LivenessState i _) = i
selectByType RFloat (LivenessState _ f) = f

dummyLivenessLoc :: LivenessLoc
dummyLivenessLoc = LivenessLoc dummyLoc (LivenessState S.empty S.empty)

store :: RegType a -> RegID -> Int -> Inst LivenessLoc RegID AllowBranch
store RInt reg local = IStore dummyLivenessLoc (savedReg RInt reg) stackReg local
store RFloat reg local = IFStore dummyLivenessLoc (savedReg RFloat reg) stackReg local

load :: RegType a -> RegID -> Int -> Inst LivenessLoc RegID AllowBranch
load RInt reg local = ILoad dummyLivenessLoc (savedReg RInt reg) stackReg local
load RFloat reg local = IFLoad dummyLivenessLoc (savedReg RFloat reg) stackReg local

genPrologueAndEpilogue :: RegType a -> LivenessState -> FunctionCallState (Seq (Inst LivenessLoc RegID AllowBranch), Seq (Inst LivenessLoc RegID AllowBranch))
genPrologueAndEpilogue rTy alive = do
    foldM
        ( \(prologue, epilogue) reg -> do
            local <- genLocalVar
            pure
                ( prologue
                    <> singleton
                        (store rTy reg local)
                , epilogue
                    <> singleton
                        (load rTy reg local)
                )
        )
        (empty, empty)
        toBeSaved
  where
    toBeSaved = selectByType rTy alive

-- | Saves registers on the stack before a function call and restores them after the call.
saveRegBeyondCall :: RegType a -> Inst LivenessLoc RegID AllowBranch -> FunctionCallState (Seq (Inst LivenessLoc RegID AllowBranch))
saveRegBeyondCall rTy (IRichCall (LivenessLoc loc alive) label iArgs fArgs) = do
    (prologue, epilogue) <- genPrologueAndEpilogue rTy alive
    pure $ prologue <> singleton (IRichCall (LivenessLoc loc alive) label iArgs fArgs) <> epilogue
saveRegBeyondCall rTy (IClosureCall (LivenessLoc loc alive) cl iArgs fArgs) = do
    (prologue, epilogue) <- genPrologueAndEpilogue rTy alive
    pure $ prologue <> singleton (IClosureCall (LivenessLoc loc alive) cl iArgs fArgs) <> epilogue
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
    saveRegBeyondCallAll :: [Inst LivenessLoc RegID AllowBranch] -> FunctionCallState (Seq (Inst LivenessLoc RegID AllowBranch))
    saveRegBeyondCallAll = foldM (\already block -> (already <>) <$> saveRegBeyondCall rTy block) empty
saveRegBeyondCall _ i = pure $ singleton i

-- | Saves registers on the stack before a function call and restores them after the call.
saveRegisters :: IntermediateCodeBlock Loc RegID -> IntermediateCodeBlock Loc RegID
saveRegisters block =
    block{localVars = locals, getICBInst = inst}
  where
    instWithLiveness = liveness $ getICBInst block
    (instList, locals) =
        runState
            ( do
                inst' <- foldl (<>) empty <$> mapM (saveRegBeyondCall RInt) instWithLiveness
                foldl (<>) empty <$> mapM (saveRegBeyondCall RFloat) inst'
            )
            $ localVars block
    inst = map (substIState livenessLoc) $ toList instList
