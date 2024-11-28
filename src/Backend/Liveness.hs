{-# LANGUAGE GADTs #-}

module Backend.Liveness (
    liveness,
    LivenessLoc (LivenessLoc, livenessLoc, livenessState),
    LivenessState (LivenessState, iAlive, fAlive),
    LivenessGraph (LivenessGraph),
    toGraph,
) where

import Backend.Asm
import Control.Monad (unless)
import Control.Monad.State (MonadState (get, put), State, evalState, gets, modify)
import Data.List (union)
import Syntax (Loc)

data LivenessState = LivenessState
    { iAlive :: [RegID]
    , fAlive :: [RegID]
    }
    deriving (Show, Eq)

data LivenessLoc = LivenessLoc
    { livenessLoc :: Loc
    , livenessState :: LivenessState
    }
    deriving (Show, Eq)

type LivenessStateM = State LivenessState

data LivenessGraph = LivenessGraph
    { iGraph :: [(RegID, RegID)]
    , fGraph :: [(RegID, RegID)]
    }
    deriving (Show, Eq)

toGraph :: [LivenessState] -> LivenessGraph
toGraph [] = LivenessGraph [] []
toGraph (s : rest) = LivenessGraph (iGraph xGraph `union` iGraph xsGraph) (fGraph xGraph `union` fGraph xsGraph)
  where
    xGraph = toGraph' s
    xsGraph = toGraph rest

    toGraph' :: LivenessState -> LivenessGraph
    toGraph' (LivenessState iAlive' fAlive') =
        LivenessGraph
            { iGraph = [(x, y) | x <- iAlive', y <- iAlive', x /= y]
            , fGraph = [(x, y) | x <- fAlive', y <- fAlive', x /= y]
            }

markUsedI :: Register RegID Int -> LivenessStateM ()
markUsedI (TempReg regId) = do
    env <- get
    unless (regId `elem` iAlive env) $
        put env{iAlive = regId : iAlive env}
markUsedI _ = pure ()

markUsedI' :: RegOrImm RegID Int -> LivenessStateM ()
markUsedI' (Reg reg) = markUsedI reg
markUsedI' (Imm _) = pure ()

markUsedF :: Register RegID Float -> LivenessStateM ()
markUsedF (TempReg regId) = do
    env <- get
    unless (regId `elem` fAlive env) $
        put env{fAlive = regId : fAlive env}
markUsedF _ = pure ()

markUsedF' :: RegOrImm RegID Float -> LivenessStateM ()
markUsedF' (Reg reg) = markUsedF reg
markUsedF' (Imm _) = pure ()

removeI :: Register RegID Int -> LivenessStateM ()
removeI (TempReg regId) = do
    modify $ \env ->
        env{iAlive = filter (/= regId) $ iAlive env}
removeI _ = pure ()

removeF :: Register RegID Float -> LivenessStateM ()
removeF (TempReg regId) = do
    modify $ \env ->
        env{fAlive = filter (/= regId) $ fAlive env}
removeF _ = pure ()

getState :: Loc -> LivenessStateM LivenessLoc
getState loc' =
    gets $ LivenessLoc loc'

liveness :: [Inst Loc RegID AllowBranch] -> [Inst LivenessLoc RegID AllowBranch]
liveness inst = reverse $ evalState (mapM liveness' $ reverse inst) $ LivenessState [] []
  where
    liveness' :: Inst Loc RegID AllowBranch -> LivenessStateM (Inst LivenessLoc RegID AllowBranch)
    liveness' (ICompOp state op dest src1 src2) = do
        state' <- getState state
        removeI dest
        markUsedI src1
        markUsedI' src2
        pure $ ICompOp state' op dest src1 src2
    liveness' (IFCompOp state op dest src1 src2) = do
        state' <- getState state
        removeI dest
        markUsedF src1
        markUsedF src2
        pure $ IFCompOp state' op dest src1 src2
    liveness' (IIntOp state op dest src1 src2) = do
        state' <- getState state
        removeI dest
        markUsedI src1
        markUsedI' src2
        pure $ IIntOp state' op dest src1 src2
    liveness' (IFOp state op dest src1 src2) = do
        state' <- getState state
        removeF dest
        markUsedF src1
        markUsedF src2
        pure $ IFOp state' op dest src1 src2
    liveness' (IMov state dest src) = do
        state' <- getState state
        removeI dest
        markUsedI' src
        pure $ IMov state' dest src
    liveness' (IFMov state dest src) = do
        state' <- getState state
        removeF dest
        markUsedF' src
        pure $ IFMov state' dest src
    liveness' (IRichCall state label iArgs fArgs) = do
        state' <- getState state
        mapM_ markUsedI iArgs
        mapM_ markUsedF fArgs
        pure $ IRichCall state' label iArgs fArgs
    liveness' (IClosureCall state cl iArgs fArgs) = do
        state' <- getState state
        markUsedI cl
        mapM_ markUsedI iArgs
        mapM_ markUsedF fArgs
        pure $ IClosureCall state' cl iArgs fArgs
    liveness' (IMakeClosure state dest label iArgs fArgs) = do
        state' <- getState state
        removeI dest
        mapM_ markUsedI iArgs
        mapM_ markUsedF fArgs
        pure $ IMakeClosure state' dest label iArgs fArgs
    liveness' (ILoad state dest src offset) = do
        state' <- getState state
        removeI dest
        markUsedI src
        pure $ ILoad state' dest src offset
    liveness' (IStore state dest src offset) = do
        state' <- getState state
        markUsedI dest
        markUsedI src
        pure $ IStore state' dest src offset
    liveness' (IFLoad state dest src offset) = do
        state' <- getState state
        removeF dest
        markUsedI src
        pure $ IFLoad state' dest src offset
    liveness' (IFStore state dest src offset) = do
        state' <- getState state
        markUsedF dest
        markUsedI src
        pure $ IFStore state' dest src offset
    liveness' (IRawInst state name retTy iArgs fArgs) = do
        state' <- getState state
        remove retTy
        mapM_ markUsedI iArgs
        mapM_ markUsedF fArgs
        pure $ IRawInst state' name retTy iArgs fArgs
      where
        remove :: RawInstRetTy RegID -> LivenessStateM ()
        remove RIRUnit = pure ()
        remove (RIRInt reg) = removeI reg
        remove (RIRFloat reg) = removeF reg
    liveness' (IBranch state op left right thenInst elseInst) = do
        env <- get
        thenInst' <- mapM liveness' $ reverse thenInst
        thenEnv <- get
        put env
        elseInst' <- mapM liveness' $ reverse elseInst
        elseEnv <- get
        put $ mergeLivenessState thenEnv elseEnv
        state' <- getState state
        markUsedI left
        markUsedI right
        pure $
            IBranch
                state'
                op
                left
                right
                (reverse thenInst')
                (reverse elseInst')
      where
        mergeLivenessState :: LivenessState -> LivenessState -> LivenessState
        mergeLivenessState (LivenessState iAlive1 fAlive1) (LivenessState iAlive2 fAlive2) =
            LivenessState (iAlive1 `union` iAlive2) (fAlive1 `union` fAlive2)
