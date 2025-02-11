{-# LANGUAGE GADTs #-}

module BackEnd.Spill (spill) where

import BackEnd.BackendEnv (BackendStateT, genTempReg)
import IR (
    AbstCodeBlock,
    AbstInst,
    HCodeBlock (hInst, localVars),
    Inst (..),
 )
import Registers (RegID, RegOrImm (Reg), RegType (RFloat, RInt), Register (Register), savedReg, stackReg)
import Syntax (dummyLoc)

loadNewReg :: (Monad m) => RegType b -> Int -> RegID -> Register a -> BackendStateT m (Register a, [AbstInst])
loadNewReg RInt vars searching victim@(Register RInt _) =
    if savedReg RInt searching == victim
        then do
            newReg <- genTempReg RInt
            pure (newReg, [ILoad dummyLoc newReg stackReg vars])
        else pure (victim, [])
loadNewReg RFloat vars searching victim@(Register RFloat _) =
    if savedReg RFloat searching == victim
        then do
            newReg <- genTempReg RFloat
            pure (newReg, [ILoad dummyLoc newReg stackReg vars])
        else pure (victim, [])
loadNewReg _ _ _ victim = pure (victim, [])

loadNewRegOrImm :: (Monad m) => RegType b -> Int -> RegID -> RegOrImm a -> BackendStateT m (RegOrImm a, [AbstInst])
loadNewRegOrImm rTy vars searching (Reg reg) = do
    (reg', inst) <- loadNewReg rTy vars searching reg
    pure (Reg reg', inst)
loadNewRegOrImm _ _ _ imm = pure (imm, [])

storeNewReg :: (Monad m) => RegType b -> Int -> RegID -> Register a -> BackendStateT m (Register a, [AbstInst])
storeNewReg RInt vars searching victim@(Register RInt _) =
    if savedReg RInt searching == victim
        then do
            newReg <- genTempReg RInt
            pure (newReg, [IStore dummyLoc newReg stackReg vars])
        else pure (victim, [])
storeNewReg RFloat vars searching victim@(Register RFloat _) =
    if savedReg RFloat searching == victim
        then do
            newReg <- genTempReg RFloat
            pure (newReg, [IStore dummyLoc newReg stackReg vars])
        else pure (victim, [])
storeNewReg _ _ _ victim = pure (victim, [])

replaceRegWithMem :: (Monad m) => RegType a -> Int -> RegID -> AbstInst -> BackendStateT m [AbstInst]
replaceRegWithMem rTy vars reg (ICompOp state op dest src1 src2) = do
    (src1', inst1) <- loadNewReg rTy vars reg src1
    (src2', inst2) <- loadNewRegOrImm rTy vars reg src2
    (dest', inst3) <- storeNewReg rTy vars reg dest
    pure $ inst1 ++ inst2 ++ [ICompOp state op dest' src1' src2'] ++ inst3
replaceRegWithMem rTy vars reg (IIntOp state op dest src1 src2) = do
    (src1', inst1) <- loadNewReg rTy vars reg src1
    (src2', inst2) <- loadNewRegOrImm rTy vars reg src2
    (dest', inst3) <- storeNewReg rTy vars reg dest
    pure $ inst1 ++ inst2 ++ [IIntOp state op dest' src1' src2'] ++ inst3
replaceRegWithMem rTy vars reg (IFOp state op dest src1 src2) = do
    (src1', inst1) <- loadNewReg rTy vars reg src1
    (src2', inst2) <- loadNewReg rTy vars reg src2
    (dest', inst3) <- storeNewReg rTy vars reg dest
    pure $ inst1 ++ inst2 ++ [IFOp state op dest' src1' src2'] ++ inst3
replaceRegWithMem rTy vars reg (IMov state dest src) = do
    (src', inst1) <- loadNewRegOrImm rTy vars reg src
    (dest', inst2) <- storeNewReg rTy vars reg dest
    pure $ inst1 ++ [IMov state dest' src'] ++ inst2
replaceRegWithMem rTy vars reg (IRichCall state label iArgs fArgs) = do
    iArgs' <- mapM (loadNewRegOrImm rTy vars reg) iArgs
    fArgs' <- mapM (loadNewReg rTy vars reg) fArgs
    let iInst = concatMap snd iArgs'
    let fInst = concatMap snd fArgs'
    pure $ iInst ++ fInst ++ [IRichCall state label (map fst iArgs') (map fst fArgs')]
replaceRegWithMem rTy vars reg (IClosureCall state cl iArgs fArgs) = do
    (cl', inst1) <- loadNewReg rTy vars reg cl
    iArgs' <- mapM (loadNewRegOrImm rTy vars reg) iArgs
    fArgs' <- mapM (loadNewReg rTy vars reg) fArgs
    let iInst2 = concatMap snd iArgs'
    let fInst2 = concatMap snd fArgs'
    pure $ inst1 ++ iInst2 ++ fInst2 ++ [IClosureCall state cl' (map fst iArgs') (map fst fArgs')]
replaceRegWithMem rTy vars reg (IMakeClosure state dest label iArgs fArgs) = do
    iArgs' <- mapM (loadNewRegOrImm rTy vars reg) iArgs
    fArgs' <- mapM (loadNewReg rTy vars reg) fArgs
    let iInst = concatMap snd iArgs'
    let fInst = concatMap snd fArgs'
    (dest', inst') <- storeNewReg rTy vars reg dest
    pure $ iInst ++ fInst ++ [IMakeClosure state dest' label (map fst iArgs') (map fst fArgs')] ++ inst'
replaceRegWithMem rTy vars reg (ILoad state dest src offset) = do
    (src', inst) <- loadNewReg rTy vars reg src
    (dest', inst') <- storeNewReg rTy vars reg dest
    pure $ inst ++ [ILoad state dest' src' offset] ++ inst'
replaceRegWithMem rTy vars reg (IStore state src dest offset) = do
    (src', inst) <- loadNewReg rTy vars reg src
    (dest', inst') <- loadNewReg rTy vars reg dest
    pure $ inst ++ inst' ++ [IStore state src' dest' offset]
replaceRegWithMem rTy vars reg (IRawInst state inst dest iArgs fArgs) = do
    iArgs' <- mapM (loadNewRegOrImm rTy vars reg) iArgs
    fArgs' <- mapM (loadNewReg rTy vars reg) fArgs
    let iInst' = concatMap snd iArgs'
    let fInst' = concatMap snd fArgs'
    (dest', inst'') <- storeNewReg rTy vars reg dest
    pure $ iInst' ++ fInst' ++ [IRawInst state inst dest' (map fst iArgs') (map fst fArgs')] ++ inst''

-- | Store the value of the register to the stack and replace it with a new one.
spill :: (Monad m) => RegType a -> RegID -> AbstCodeBlock -> BackendStateT m AbstCodeBlock
spill rTy reg block = do
    inst' <- mapM (replaceRegWithMem rTy (localVars block) reg) (hInst block)
    pure $ block{localVars = localVars block + 1, hInst = concat inst'}
