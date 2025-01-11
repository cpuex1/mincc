{-# LANGUAGE GADTs #-}

module BackEnd.Spill (spillI, spillF) where

import BackEnd.BackendEnv (BackendStateT, genTempReg)
import IR (
    AllowBranch,
    Inst (..),
    IntermediateCodeBlock (getICBInst, localVars),
    RawInstRetTy (RIRFloat, RIRInt),
    RegID,
 )
import Registers (RegOrImm (Reg), RegType (RFloat, RInt), Register, savedReg, stackReg)
import Syntax (Loc, dummyLoc)

loadNewReg :: (Monad m) => RegType a -> Int -> RegID -> Register RegID a -> BackendStateT m (Register RegID a, [Inst Loc RegID AllowBranch])
loadNewReg RInt vars searching victim =
    if savedReg RInt searching == victim
        then do
            newReg <- genTempReg RInt
            pure (newReg, [ILoad dummyLoc newReg stackReg vars])
        else pure (victim, [])
loadNewReg RFloat vars searching victim =
    if savedReg RFloat searching == victim
        then do
            newReg <- genTempReg RFloat
            pure (newReg, [IFLoad dummyLoc newReg stackReg vars])
        else pure (victim, [])

loadNewRegOrImm :: (Monad m) => RegType a -> Int -> RegID -> RegOrImm RegID a -> BackendStateT m (RegOrImm RegID a, [Inst Loc RegID AllowBranch])
loadNewRegOrImm RInt vars searching victim =
    if Reg (savedReg RInt searching) == victim
        then do
            newReg <- genTempReg RInt
            pure (Reg newReg, [ILoad dummyLoc newReg stackReg vars])
        else pure (victim, [])
loadNewRegOrImm RFloat vars searching victim =
    if Reg (savedReg RFloat searching) == victim
        then do
            newReg <- genTempReg RFloat
            pure (Reg newReg, [IFLoad dummyLoc newReg stackReg vars])
        else pure (victim, [])

storeNewReg :: (Monad m) => RegType a -> Int -> RegID -> Register RegID a -> BackendStateT m (Register RegID a, [Inst Loc RegID AllowBranch])
storeNewReg RInt vars searching victim =
    if savedReg RInt searching == victim
        then do
            newReg <- genTempReg RInt
            pure (newReg, [IStore dummyLoc newReg stackReg vars])
        else pure (victim, [])
storeNewReg RFloat vars searching victim =
    if savedReg RFloat searching == victim
        then do
            newReg <- genTempReg RFloat
            pure (newReg, [IFStore dummyLoc newReg stackReg vars])
        else pure (victim, [])

replaceIRegWithMem :: (Monad m) => Int -> RegID -> Inst Loc RegID AllowBranch -> BackendStateT m [Inst Loc RegID AllowBranch]
replaceIRegWithMem vars reg (ICompOp state op dest src1 src2) = do
    (src1', inst1) <- loadNewReg RInt vars reg src1
    (src2', inst2) <- loadNewRegOrImm RInt vars reg src2
    (dest', inst3) <- storeNewReg RInt vars reg dest
    pure $ inst1 ++ inst2 ++ [ICompOp state op dest' src1' src2'] ++ inst3
replaceIRegWithMem _ _ (IFCompOp state op dest src1 src2) =
    pure [IFCompOp state op dest src1 src2]
replaceIRegWithMem vars reg (IIntOp state op dest src1 src2) = do
    (src1', inst1) <- loadNewReg RInt vars reg src1
    (src2', inst2) <- loadNewRegOrImm RInt vars reg src2
    (dest', inst3) <- storeNewReg RInt vars reg dest
    pure $ inst1 ++ inst2 ++ [IIntOp state op dest' src1' src2'] ++ inst3
replaceIRegWithMem _ _ (IFOp state op dest src1 src2) =
    pure [IFOp state op dest src1 src2]
replaceIRegWithMem vars reg (IMov state dest src) = do
    (src', inst1) <- loadNewRegOrImm RInt vars reg src
    (dest', inst2) <- storeNewReg RInt vars reg dest
    pure $ inst1 ++ [IMov state dest' src'] ++ inst2
replaceIRegWithMem _ _ (IFMov state dest src) = do
    pure [IFMov state dest src]
replaceIRegWithMem vars reg (IRichCall state label iArgs fArgs) = do
    iArgs' <- mapM (loadNewRegOrImm RInt vars reg) iArgs
    let inst = concatMap snd iArgs'
    pure $ inst ++ [IRichCall state label (map fst iArgs') fArgs]
replaceIRegWithMem vars reg (IClosureCall state cl iArgs fArgs) = do
    (cl', inst1) <- loadNewReg RInt vars reg cl
    iArgs' <- mapM (loadNewRegOrImm RInt vars reg) iArgs
    let inst2 = concatMap snd iArgs'
    pure $ inst1 ++ inst2 ++ [IClosureCall state cl' (map fst iArgs') fArgs]
replaceIRegWithMem vars reg (IMakeClosure state dest label iArgs fArgs) = do
    iArgs' <- mapM (loadNewRegOrImm RInt vars reg) iArgs
    let inst = concatMap snd iArgs'
    (dest', inst') <- storeNewReg RInt vars reg dest
    pure $ inst ++ [IMakeClosure state dest' label (map fst iArgs') fArgs] ++ inst'
replaceIRegWithMem vars reg (ILoad state dest src offset) = do
    (src', inst) <- loadNewReg RInt vars reg src
    (dest', inst') <- storeNewReg RInt vars reg dest
    pure $ inst ++ [ILoad state dest' src' offset] ++ inst'
replaceIRegWithMem vars reg (IStore state src dest offset) = do
    (src', inst) <- loadNewReg RInt vars reg src
    (dest', inst') <- loadNewReg RInt vars reg dest
    pure $ inst ++ inst' ++ [IStore state src' dest' offset]
replaceIRegWithMem vars reg (IFLoad state dest src offset) = do
    (src', inst) <- loadNewReg RInt vars reg src
    pure $ inst ++ [IFLoad state dest src' offset]
replaceIRegWithMem vars reg (IFStore state dest src offset) = do
    (src', inst) <- loadNewReg RInt vars reg src
    pure $ inst ++ [IFStore state dest src' offset]
replaceIRegWithMem vars reg (IRawInst state inst (RIRInt dest) iArgs fArgs) = do
    iArgs' <- mapM (loadNewRegOrImm RInt vars reg) iArgs
    let inst' = concatMap snd iArgs'
    (dest', inst'') <- storeNewReg RInt vars reg dest
    pure $ inst' ++ [IRawInst state inst (RIRInt dest') (map fst iArgs') fArgs] ++ inst''
replaceIRegWithMem vars reg (IRawInst state inst dest iArgs fArgs) = do
    iArgs' <- mapM (loadNewRegOrImm RInt vars reg) iArgs
    let inst' = concatMap snd iArgs'
    pure $ inst' ++ [IRawInst state inst dest (map fst iArgs') fArgs]
replaceIRegWithMem vars reg (IBranch state op src1 src2 inst1 inst2) = do
    (src1', inst1') <- loadNewReg RInt vars reg src1
    (src2', inst2') <- loadNewReg RInt vars reg src2
    inst3 <- mapM (replaceIRegWithMem vars reg) inst1
    let inst3' = concat inst3
    inst4 <- mapM (replaceIRegWithMem vars reg) inst2
    let inst4' = concat inst4
    pure $ inst1' ++ inst2' ++ [IBranch state op src1' src2' inst3' inst4']

replaceFRegWithMem :: (Monad m) => Int -> RegID -> Inst Loc RegID AllowBranch -> BackendStateT m [Inst Loc RegID AllowBranch]
replaceFRegWithMem _ _ (ICompOp state op dest src1 src2) =
    pure [ICompOp state op dest src1 src2]
replaceFRegWithMem vars reg (IFCompOp state op dest src1 src2) = do
    (src1', inst1) <- loadNewReg RFloat vars reg src1
    (src2', inst2) <- loadNewReg RFloat vars reg src2
    pure $ inst1 ++ inst2 ++ [IFCompOp state op dest src1' src2']
replaceFRegWithMem _ _ (IIntOp state op dest src1 src2) =
    pure [IIntOp state op dest src1 src2]
replaceFRegWithMem vars reg (IFOp state op dest src1 src2) = do
    (src1', inst1) <- loadNewReg RFloat vars reg src1
    (src2', inst2) <- loadNewReg RFloat vars reg src2
    (dest', inst3) <- storeNewReg RFloat vars reg dest
    pure $ inst1 ++ inst2 ++ [IFOp state op dest' src1' src2'] ++ inst3
replaceFRegWithMem _ _ (IMov state dest src) =
    pure [IMov state dest src]
replaceFRegWithMem vars reg (IFMov state dest src) = do
    (src', inst1) <- loadNewRegOrImm RFloat vars reg src
    (dest', inst2) <- storeNewReg RFloat vars reg dest
    pure $ inst1 ++ [IFMov state dest' src'] ++ inst2
replaceFRegWithMem vars reg (IRichCall state label iArgs fArgs) = do
    fArgs' <- mapM (loadNewReg RFloat vars reg) fArgs
    let inst = concatMap snd fArgs'
    pure $ inst ++ [IRichCall state label iArgs (map fst fArgs')]
replaceFRegWithMem vars reg (IClosureCall state cl iArgs fArgs) = do
    fArgs' <- mapM (loadNewReg RFloat vars reg) fArgs
    let inst = concatMap snd fArgs'
    pure $ inst ++ [IClosureCall state cl iArgs (map fst fArgs')]
replaceFRegWithMem vars reg (IMakeClosure state dest label iArgs fArgs) = do
    fArgs' <- mapM (loadNewReg RFloat vars reg) fArgs
    let inst = concatMap snd fArgs'
    pure $ inst ++ [IMakeClosure state dest label iArgs (map fst fArgs')]
replaceFRegWithMem _ _ (ILoad state dest src offset) =
    pure [ILoad state dest src offset]
replaceFRegWithMem _ _ (IStore state dest src offset) =
    pure [IStore state dest src offset]
replaceFRegWithMem vars reg (IFLoad state dest src offset) = do
    (dest', inst) <- storeNewReg RFloat vars reg dest
    pure $ IFLoad state dest' src offset : inst
replaceFRegWithMem vars reg (IFStore state dest src offset) = do
    (dest', inst') <- loadNewReg RFloat vars reg dest
    pure $ inst' ++ [IFStore state dest' src offset]
replaceFRegWithMem vars reg (IRawInst state inst (RIRFloat dest) iArgs fArgs) = do
    fArgs' <- mapM (loadNewReg RFloat vars reg) fArgs
    let inst' = concatMap snd fArgs'
    (dest', inst'') <- storeNewReg RFloat vars reg dest
    pure $ inst' ++ [IRawInst state inst (RIRFloat dest') iArgs (map fst fArgs')] ++ inst''
replaceFRegWithMem vars reg (IRawInst state inst dest iArgs fArgs) = do
    fArgs' <- mapM (loadNewReg RFloat vars reg) fArgs
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
