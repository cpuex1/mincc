{-# LANGUAGE GADTs #-}

module BackEnd.Liveness (
    liveness,
    RegGraph (..),
    LivenessLoc (LivenessLoc, livenessLoc, livenessState),
    LivenessState (LivenessState, iAlive, fAlive),
    LivenessGraph (LivenessGraph),
    toGraph,
) where

import Control.Monad.State (MonadState (get, put), State, evalState, gets, modify)
import Data.Map (Map, fromList)
import Data.Set (Set, delete, empty, insert, toAscList, union, unions)
import IR (
    AllowBranch,
    Inst (..),
    RawInstRetTy (..),
    RegID,
 )
import Registers (RegOrImm (Reg), RegType (RFloat, RInt), Register (Register), RegisterKind (SavedReg))
import Syntax (Loc)

data LivenessState = LivenessState
    { iAlive :: Set RegID
    , fAlive :: Set RegID
    }
    deriving (Show, Eq)

data LivenessLoc = LivenessLoc
    { livenessLoc :: Loc
    , livenessState :: LivenessState
    }
    deriving (Show, Eq)

type LivenessStateM = State LivenessState

data RegGraph = RegGraph
    { vertices :: Set RegID
    , edges :: Map RegID (Set RegID)
    }
    deriving (Show, Eq)

data LivenessGraph = LivenessGraph
    { iGraph :: RegGraph
    , fGraph :: RegGraph
    }
    deriving (Show, Eq)

toGraph :: [LivenessState] -> LivenessGraph
toGraph states = LivenessGraph (RegGraph iV iE) (RegGraph fV fE)
  where
    iState = map iAlive states
    fState = map fAlive states

    iV = unions iState
    fV = unions fState

    iE = fromList $ map (\v -> (v, unions (filter (elem v) iState))) (toAscList iV)
    fE = fromList $ map (\v -> (v, unions (filter (elem v) fState))) (toAscList fV)

markUsed :: RegType a -> Register RegID a -> LivenessStateM ()
markUsed RInt (Register RInt (SavedReg regId)) = do
    env <- get
    put env{iAlive = insert regId $ iAlive env}
markUsed RFloat (Register RFloat (SavedReg regId)) = do
    env <- get
    put env{fAlive = insert regId $ fAlive env}
markUsed _ _ = pure ()

markUsedImm :: RegType a -> RegOrImm RegID a -> LivenessStateM ()
markUsedImm rTy (Reg reg) = markUsed rTy reg
markUsedImm _ _ = pure ()

remove :: RegType a -> Register RegID a -> LivenessStateM ()
remove RInt (Register RInt (SavedReg regId)) = do
    modify $ \env ->
        env{iAlive = delete regId $ iAlive env}
remove RFloat (Register RFloat (SavedReg regId)) = do
    modify $ \env ->
        env{fAlive = delete regId $ fAlive env}
remove _ _ = pure ()

getState :: Loc -> LivenessStateM LivenessLoc
getState loc' =
    gets $ LivenessLoc loc'

-- | Calculates liveness information for each instruction.
liveness :: [Inst Loc RegID AllowBranch] -> [Inst LivenessLoc RegID AllowBranch]
liveness inst = reverse $ evalState (mapM liveness' $ reverse inst) $ LivenessState empty empty
  where
    liveness' :: Inst Loc RegID AllowBranch -> LivenessStateM (Inst LivenessLoc RegID AllowBranch)
    liveness' (ICompOp state op dest src1 src2) = do
        state' <- getState state
        remove RInt dest
        markUsed RInt src1
        markUsedImm RInt src2
        pure $ ICompOp state' op dest src1 src2
    liveness' (IFCompOp state op dest src1 src2) = do
        state' <- getState state
        remove RInt dest
        markUsed RFloat src1
        markUsed RFloat src2
        pure $ IFCompOp state' op dest src1 src2
    liveness' (IIntOp state op dest src1 src2) = do
        state' <- getState state
        remove RInt dest
        markUsed RInt src1
        markUsedImm RInt src2
        pure $ IIntOp state' op dest src1 src2
    liveness' (IFOp state op dest src1 src2) = do
        state' <- getState state
        remove RFloat dest
        markUsed RFloat src1
        markUsed RFloat src2
        pure $ IFOp state' op dest src1 src2
    liveness' (IMov state dest src) = do
        state' <- getState state
        remove RInt dest
        markUsedImm RInt src
        pure $ IMov state' dest src
    liveness' (IFMov state dest src) = do
        state' <- getState state
        remove RFloat dest
        markUsedImm RFloat src
        pure $ IFMov state' dest src
    liveness' (IRichCall state label iArgs fArgs) = do
        state' <- getState state
        mapM_ (markUsedImm RInt) iArgs
        mapM_ (markUsed RFloat) fArgs
        pure $ IRichCall state' label iArgs fArgs
    liveness' (IClosureCall state cl iArgs fArgs) = do
        state' <- getState state
        markUsed RInt cl
        mapM_ (markUsedImm RInt) iArgs
        mapM_ (markUsed RFloat) fArgs
        pure $ IClosureCall state' cl iArgs fArgs
    liveness' (IMakeClosure state dest label iArgs fArgs) = do
        state' <- getState state
        remove RInt dest
        mapM_ (markUsedImm RInt) iArgs
        mapM_ (markUsed RFloat) fArgs
        pure $ IMakeClosure state' dest label iArgs fArgs
    liveness' (ILoad state dest src offset) = do
        state' <- getState state
        remove RInt dest
        markUsed RInt src
        pure $ ILoad state' dest src offset
    liveness' (IStore state dest src offset) = do
        state' <- getState state
        markUsed RInt dest
        markUsed RInt src
        pure $ IStore state' dest src offset
    liveness' (IFLoad state dest src offset) = do
        state' <- getState state
        remove RFloat dest
        markUsed RInt src
        pure $ IFLoad state' dest src offset
    liveness' (IFStore state dest src offset) = do
        state' <- getState state
        markUsed RFloat dest
        markUsed RInt src
        pure $ IFStore state' dest src offset
    liveness' (IRawInst state name retTy iArgs fArgs) = do
        state' <- getState state
        removeRet retTy
        mapM_ (markUsedImm RInt) iArgs
        mapM_ (markUsed RFloat) fArgs
        pure $ IRawInst state' name retTy iArgs fArgs
      where
        removeRet :: RawInstRetTy RegID -> LivenessStateM ()
        removeRet RIRUnit = pure ()
        removeRet (RIRInt reg) = remove RInt reg
        removeRet (RIRFloat reg) = remove RFloat reg
    liveness' (IBranch state op left right thenInst elseInst) = do
        env <- get
        thenInst' <- mapM liveness' $ reverse thenInst
        thenEnv <- get
        put env
        elseInst' <- mapM liveness' $ reverse elseInst
        elseEnv <- get
        put $ mergeLivenessState thenEnv elseEnv
        state' <- getState state
        markUsed RInt left
        markUsed RInt right
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
