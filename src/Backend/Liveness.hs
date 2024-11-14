{-# LANGUAGE GADTs #-}

module Backend.Liveness (liveness, LivenessLoc(livenessState), LivenessState(iAlived, fAlived)) where

import Backend.Asm
import Backend.BackendEnv (RegID)
import Control.Monad (unless)
import Control.Monad.State (MonadState (get, put), State, evalState, gets, modify)
import Syntax (Loc)

data LivenessState = LivenessState
    { iAlived :: [RegID]
    , fAlived :: [RegID]
    }
    deriving (Show, Eq)

data LivenessLoc = LivenessLoc
    { loc :: Loc
    , livenessState :: LivenessState
    }
    deriving (Show, Eq)

type LivenessStateM = State LivenessState

mergeLivenessState :: LivenessState -> LivenessState -> LivenessState
mergeLivenessState (LivenessState iAlived1 fAlived1) (LivenessState iAlived2 fAlived2) =
    LivenessState (merge iAlived1 iAlived2) (merge fAlived1 fAlived2)
  where
    merge :: (Eq a) => [a] -> [a] -> [a]
    merge [] arr = arr
    merge (x : xs) arr =
        if x `elem` arr
            then
                merge xs arr
            else
                merge xs (x : arr)

markUsedI :: Register RegID Int -> LivenessStateM ()
markUsedI (TempReg regId) = do
    env <- get
    unless (regId `elem` iAlived env) $
        put env{iAlived = regId : iAlived env}
markUsedI _ = pure ()

markUsedI' :: RegOrImm RegID Int -> LivenessStateM ()
markUsedI' (Reg reg) = markUsedI reg
markUsedI' (Imm _) = pure ()

markUsedF :: Register RegID Float -> LivenessStateM ()
markUsedF (TempReg regId) = do
    env <- get
    unless (regId `elem` fAlived env) $
        put env{fAlived = regId : fAlived env}
markUsedF _ = pure ()

markUsedF' :: RegOrImm RegID Float -> LivenessStateM ()
markUsedF' (Reg reg) = markUsedF reg
markUsedF' (Imm _) = pure ()

removeI :: Register RegID Int -> LivenessStateM ()
removeI (TempReg regId) = do
    modify $ \env ->
        env{iAlived = filter (/= regId) $ iAlived env}
removeI _ = pure ()

removeF :: Register RegID Float -> LivenessStateM ()
removeF (TempReg regId) = do
    modify $ \env ->
        env{fAlived = filter (/= regId) $ fAlived env}
removeF _ = pure ()

getState :: Loc -> LivenessStateM LivenessLoc
getState loc' =
    gets $ LivenessLoc loc'

liveness :: [Inst Loc RegID AllowBranch] -> [Inst LivenessLoc RegID AllowBranch]
liveness inst = reverse $ evalState (mapM liveness' $ reverse inst) $ LivenessState [] []
  where
    liveness' :: Inst Loc RegID AllowBranch -> LivenessStateM (Inst LivenessLoc RegID AllowBranch)
    liveness' (ICompOp state op dest src1 src2) = do
        removeI dest
        markUsedI src1
        markUsedI' src2
        state' <- getState state
        pure $ ICompOp state' op dest src1 src2
    liveness' (IFCompOp state op dest src1 src2) = do
        removeI dest
        markUsedF src1
        markUsedF src2
        state' <- getState state
        pure $ IFCompOp state' op dest src1 src2
    liveness' (IIntOp state op dest src1 src2) = do
        removeI dest
        markUsedI src1
        markUsedI' src2
        state' <- getState state
        pure $ IIntOp state' op dest src1 src2
    liveness' (IFOp state op dest src1 src2) = do
        removeF dest
        markUsedF src1
        markUsedF src2
        state' <- getState state
        pure $ IFOp state' op dest src1 src2
    liveness' (IMov state dest src) = do
        removeI dest
        markUsedI' src
        state' <- getState state
        pure $ IMov state' dest src
    liveness' (IFMov state dest src) = do
        removeF dest
        markUsedF' src
        state' <- getState state
        pure $ IFMov state' dest src
    liveness' (IRichCall state label iArgs fArgs) = do
        mapM_ markUsedI iArgs
        mapM_ markUsedF fArgs
        state' <- getState state
        pure $ IRichCall state' label iArgs fArgs
    liveness' (IClosureCall state cl iArgs fArgs) = do
        markUsedI cl
        mapM_ markUsedI iArgs
        mapM_ markUsedF fArgs
        state' <- getState state
        pure $ IClosureCall state' cl iArgs fArgs
    liveness' (IMakeClosure state dest label iArgs fArgs) = do
        removeI dest
        mapM_ markUsedI iArgs
        mapM_ markUsedF fArgs
        state' <- getState state
        pure $ IMakeClosure state' dest label iArgs fArgs
    liveness' (ILoad state dest src offset) = do
        removeI dest
        markUsedI src
        state' <- getState state
        pure $ ILoad state' dest src offset
    liveness' (IStore state dest src offset) = do
        markUsedI dest
        markUsedI src
        state' <- getState state
        pure $ IStore state' dest src offset
    liveness' (IFLoad state dest src offset) = do
        removeF dest
        markUsedI src
        state' <- getState state
        pure $ IFLoad state' dest src offset
    liveness' (IFStore state dest src offset) = do
        markUsedF dest
        markUsedI src
        state' <- getState state
        pure $ IFStore state' dest src offset
    liveness' (IBranch state op left right thenInst elseInst) = do
        env <- get
        thenInst' <- mapM liveness' $ reverse thenInst
        thenEnv <- get
        put env
        elseInst' <- mapM liveness' $ reverse elseInst
        elseEnv <- get
        put $ mergeLivenessState thenEnv elseEnv
        markUsedI left
        markUsedI right
        state' <- getState state
        pure $
            IBranch
                state'
                op
                left
                right
                (reverse thenInst')
                (reverse elseInst')
