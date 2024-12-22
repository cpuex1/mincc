{-# LANGUAGE GADTs #-}

module Backend.Spill (spillI, spillF) where

import Backend.Asm (
    AllowBranch,
    Inst (..),
    IntermediateCodeBlock (getICBInst, localVars),
    RawInstRetTy (RIRFloat, RIRInt),
    RegID,
    RegOrImm (Reg),
    Register (SavedReg, StackReg),
 )
import Backend.BackendEnv (BackendStateT, genTempFReg, genTempIReg)
import Syntax (Loc, dummyLoc)

loadNewIReg :: (Monad m) => Int -> RegID -> Register RegID Int -> BackendStateT m (Register RegID Int, [Inst Loc RegID AllowBranch])
loadNewIReg vars searching victim =
    if SavedReg searching == victim
        then do
            newReg <- genTempIReg
            pure (newReg, [ILoad dummyLoc newReg StackReg vars])
        else pure (victim, [])

loadNewIReg' :: (Monad m) => Int -> RegID -> RegOrImm RegID Int -> BackendStateT m (RegOrImm RegID Int, [Inst Loc RegID AllowBranch])
loadNewIReg' vars searching victim =
    if Reg (SavedReg searching) == victim
        then do
            newReg <- genTempIReg
            pure (Reg newReg, [ILoad dummyLoc newReg StackReg vars])
        else pure (victim, [])

loadNewFReg :: (Monad m) => Int -> RegID -> Register RegID Float -> BackendStateT m (Register RegID Float, [Inst Loc RegID AllowBranch])
loadNewFReg vars searching victim =
    if SavedReg searching == victim
        then do
            newReg <- genTempFReg
            pure (newReg, [IFLoad dummyLoc newReg StackReg vars])
        else pure (victim, [])

loadNewFReg' :: (Monad m) => Int -> RegID -> RegOrImm RegID Float -> BackendStateT m (RegOrImm RegID Float, [Inst Loc RegID AllowBranch])
loadNewFReg' vars searching victim =
    if Reg (SavedReg searching) == victim
        then do
            newReg <- genTempFReg
            pure (Reg newReg, [IFLoad dummyLoc newReg StackReg vars])
        else pure (victim, [])

storeNewIReg :: (Monad m) => Int -> RegID -> Register RegID Int -> BackendStateT m (Register RegID Int, [Inst Loc RegID AllowBranch])
storeNewIReg vars searching victim =
    if SavedReg searching == victim
        then do
            newReg <- genTempIReg
            pure (newReg, [IStore dummyLoc newReg StackReg vars])
        else pure (victim, [])

storeNewFReg :: (Monad m) => Int -> RegID -> Register RegID Float -> BackendStateT m (Register RegID Float, [Inst Loc RegID AllowBranch])
storeNewFReg vars searching victim =
    if SavedReg searching == victim
        then do
            newReg <- genTempFReg
            pure (newReg, [IFStore dummyLoc newReg StackReg vars])
        else pure (victim, [])

replaceIRegWithMem :: (Monad m) => Int -> RegID -> Inst Loc RegID AllowBranch -> BackendStateT m [Inst Loc RegID AllowBranch]
replaceIRegWithMem vars reg (ICompOp state op dest src1 src2) = do
    (src1', inst1) <- loadNewIReg vars reg src1
    (src2', inst2) <- loadNewIReg' vars reg src2
    (dest', inst3) <- storeNewIReg vars reg dest
    pure $ inst1 ++ inst2 ++ [ICompOp state op dest' src1' src2'] ++ inst3
replaceIRegWithMem _ _ (IFCompOp state op dest src1 src2) =
    pure [IFCompOp state op dest src1 src2]
replaceIRegWithMem vars reg (IIntOp state op dest src1 src2) = do
    (src1', inst1) <- loadNewIReg vars reg src1
    (src2', inst2) <- loadNewIReg' vars reg src2
    (dest', inst3) <- storeNewIReg vars reg dest
    pure $ inst1 ++ inst2 ++ [IIntOp state op dest' src1' src2'] ++ inst3
replaceIRegWithMem _ _ (IFOp state op dest src1 src2) =
    pure [IFOp state op dest src1 src2]
replaceIRegWithMem vars reg (IMov state dest src) = do
    (src', inst1) <- loadNewIReg' vars reg src
    (dest', inst2) <- storeNewIReg vars reg dest
    pure $ inst1 ++ [IMov state dest' src'] ++ inst2
replaceIRegWithMem _ _ (IFMov state dest src) = do
    pure [IFMov state dest src]
replaceIRegWithMem vars reg (IRichCall state label iArgs fArgs) = do
    iArgs' <- mapM (loadNewIReg' vars reg) iArgs
    let inst = concatMap snd iArgs'
    pure $ inst ++ [IRichCall state label (map fst iArgs') fArgs]
replaceIRegWithMem vars reg (IClosureCall state cl iArgs fArgs) = do
    (cl', inst1) <- loadNewIReg vars reg cl
    iArgs' <- mapM (loadNewIReg' vars reg) iArgs
    let inst2 = concatMap snd iArgs'
    pure $ inst1 ++ inst2 ++ [IClosureCall state cl' (map fst iArgs') fArgs]
replaceIRegWithMem vars reg (IMakeClosure state dest label iArgs fArgs) = do
    iArgs' <- mapM (loadNewIReg' vars reg) iArgs
    let inst = concatMap snd iArgs'
    (dest', inst') <- storeNewIReg vars reg dest
    pure $ inst ++ [IMakeClosure state dest' label (map fst iArgs') fArgs] ++ inst'
replaceIRegWithMem vars reg (ILoad state dest src offset) = do
    (src', inst) <- loadNewIReg vars reg src
    (dest', inst') <- storeNewIReg vars reg dest
    pure $ inst ++ [ILoad state dest' src' offset] ++ inst'
replaceIRegWithMem vars reg (IStore state src dest offset) = do
    (src', inst) <- loadNewIReg vars reg src
    (dest', inst') <- loadNewIReg vars reg dest
    pure $ inst ++ inst' ++ [IStore state src' dest' offset]
replaceIRegWithMem vars reg (IFLoad state dest src offset) = do
    (src', inst) <- loadNewIReg vars reg src
    pure $ inst ++ [IFLoad state dest src' offset]
replaceIRegWithMem vars reg (IFStore state dest src offset) = do
    (src', inst) <- loadNewIReg vars reg src
    pure $ inst ++ [IFStore state dest src' offset]
replaceIRegWithMem vars reg (IRawInst state inst (RIRInt dest) iArgs fArgs) = do
    iArgs' <- mapM (loadNewIReg' vars reg) iArgs
    let inst' = concatMap snd iArgs'
    (dest', inst'') <- storeNewIReg vars reg dest
    pure $ inst' ++ [IRawInst state inst (RIRInt dest') (map fst iArgs') fArgs] ++ inst''
replaceIRegWithMem vars reg (IRawInst state inst dest iArgs fArgs) = do
    iArgs' <- mapM (loadNewIReg' vars reg) iArgs
    let inst' = concatMap snd iArgs'
    pure $ inst' ++ [IRawInst state inst dest (map fst iArgs') fArgs]
replaceIRegWithMem vars reg (IBranch state op src1 src2 inst1 inst2) = do
    (src1', inst1') <- loadNewIReg vars reg src1
    (src2', inst2') <- loadNewIReg vars reg src2
    inst3 <- mapM (replaceIRegWithMem vars reg) inst1
    let inst3' = concat inst3
    inst4 <- mapM (replaceIRegWithMem vars reg) inst2
    let inst4' = concat inst4
    pure $ inst1' ++ inst2' ++ [IBranch state op src1' src2' inst3' inst4']

replaceFRegWithMem :: (Monad m) => Int -> RegID -> Inst Loc RegID AllowBranch -> BackendStateT m [Inst Loc RegID AllowBranch]
replaceFRegWithMem _ _ (ICompOp state op dest src1 src2) =
    pure [ICompOp state op dest src1 src2]
replaceFRegWithMem vars reg (IFCompOp state op dest src1 src2) = do
    (src1', inst1) <- loadNewFReg vars reg src1
    (src2', inst2) <- loadNewFReg vars reg src2
    pure $ inst1 ++ inst2 ++ [IFCompOp state op dest src1' src2']
replaceFRegWithMem _ _ (IIntOp state op dest src1 src2) =
    pure [IIntOp state op dest src1 src2]
replaceFRegWithMem vars reg (IFOp state op dest src1 src2) = do
    (src1', inst1) <- loadNewFReg vars reg src1
    (src2', inst2) <- loadNewFReg vars reg src2
    (dest', inst3) <- storeNewFReg vars reg dest
    pure $ inst1 ++ inst2 ++ [IFOp state op dest' src1' src2'] ++ inst3
replaceFRegWithMem _ _ (IMov state dest src) =
    pure [IMov state dest src]
replaceFRegWithMem vars reg (IFMov state dest src) = do
    (src', inst1) <- loadNewFReg' vars reg src
    (dest', inst2) <- storeNewFReg vars reg dest
    pure $ inst1 ++ [IFMov state dest' src'] ++ inst2
replaceFRegWithMem vars reg (IRichCall state label iArgs fArgs) = do
    fArgs' <- mapM (loadNewFReg vars reg) fArgs
    let inst = concatMap snd fArgs'
    pure $ inst ++ [IRichCall state label iArgs (map fst fArgs')]
replaceFRegWithMem vars reg (IClosureCall state cl iArgs fArgs) = do
    fArgs' <- mapM (loadNewFReg vars reg) fArgs
    let inst = concatMap snd fArgs'
    pure $ inst ++ [IClosureCall state cl iArgs (map fst fArgs')]
replaceFRegWithMem vars reg (IMakeClosure state dest label iArgs fArgs) = do
    fArgs' <- mapM (loadNewFReg vars reg) fArgs
    let inst = concatMap snd fArgs'
    pure $ inst ++ [IMakeClosure state dest label iArgs (map fst fArgs')]
replaceFRegWithMem _ _ (ILoad state dest src offset) =
    pure [ILoad state dest src offset]
replaceFRegWithMem _ _ (IStore state dest src offset) =
    pure [IStore state dest src offset]
replaceFRegWithMem vars reg (IFLoad state dest src offset) = do
    (dest', inst) <- storeNewFReg vars reg dest
    pure $ IFLoad state dest' src offset : inst
replaceFRegWithMem vars reg (IFStore state dest src offset) = do
    (dest', inst') <- loadNewFReg vars reg dest
    pure $ inst' ++ [IFStore state dest' src offset]
replaceFRegWithMem vars reg (IRawInst state inst (RIRFloat dest) iArgs fArgs) = do
    fArgs' <- mapM (loadNewFReg vars reg) fArgs
    let inst' = concatMap snd fArgs'
    (dest', inst'') <- storeNewFReg vars reg dest
    pure $ inst' ++ [IRawInst state inst (RIRFloat dest') iArgs (map fst fArgs')] ++ inst''
replaceFRegWithMem vars reg (IRawInst state inst dest iArgs fArgs) = do
    fArgs' <- mapM (loadNewFReg vars reg) fArgs
    let inst' = concatMap snd fArgs'
    pure $ inst' ++ [IRawInst state inst dest iArgs (map fst fArgs')]
replaceFRegWithMem vars reg (IBranch state op src1 src2 inst1 inst2) = do
    inst3 <- mapM (replaceFRegWithMem vars reg) inst1
    let inst3' = concat inst3
    inst4 <- mapM (replaceFRegWithMem vars reg) inst2
    let inst4' = concat inst4
    pure [IBranch state op src1 src2 inst3' inst4']

-- | Store the value of the int register to the stack and replace it with a new one.
spillI :: (Monad m) => RegID -> IntermediateCodeBlock Loc RegID -> BackendStateT m (IntermediateCodeBlock Loc RegID)
spillI reg block = do
    inst' <- mapM (replaceIRegWithMem (localVars block) reg) (getICBInst block)
    pure $ block{localVars = localVars block + 1, getICBInst = concat inst'}

-- | Store the value of the float register to the stack and replace it with a new one.
spillF :: (Monad m) => RegID -> IntermediateCodeBlock Loc RegID -> BackendStateT m (IntermediateCodeBlock Loc RegID)
spillF reg block = do
    inst' <- mapM (replaceFRegWithMem (localVars block) reg) (getICBInst block)
    pure $ block{localVars = localVars block + 1, getICBInst = concat inst'}
