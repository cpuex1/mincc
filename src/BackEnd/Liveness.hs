{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module BackEnd.Liveness (
    liveness,
    Liveness (..),
    RegGraph (..),
    LivenessLoc (..),
    LivenessGraph,
    LivenessInst,
    LivenessCodeBlock,
    toGraph,
) where

import Control.Monad.State (MonadState (get, put), State, evalState, gets, modify)
import Data.Map (Map, fromList)
import Data.Set (Set, delete, empty, insert, toAscList, union, unions)
import Data.Text (intercalate)
import Display (Display (display))
import IR (
    AbstInst,
    HCodeBlock,
    Inst (..),
    InstKind (..),
 )
import Registers (
    RegID,
    RegOrImm (Reg),
    RegType (RFloat, RInt),
    RegVariant (RegVariant),
    Register (Register),
    RegisterKind (SavedReg),
    updateVariant,
    (#!!),
 )
import Syntax (Loc)

newtype Liveness a = Liveness
    { alive :: Set RegID
    }
    deriving (Show, Eq)

instance Semigroup (Liveness a) where
    Liveness a <> Liveness b = Liveness $ a `union` b

instance Monoid (Liveness a) where
    mempty = Liveness empty

instance Display (RegVariant Liveness) where
    display (RegVariant (Liveness iAlive) (Liveness fAlive)) =
        "["
            <> intercalate "," (map (display . Register RInt . SavedReg) (toAscList iAlive))
            <> "], ["
            <> intercalate "," (map (display . Register RFloat . SavedReg) (toAscList fAlive))
            <> "]"

data LivenessLoc = LivenessLoc
    { livenessLoc :: Loc
    , livenessProp :: RegVariant Liveness
    }
    deriving (Show, Eq)

instance Display LivenessLoc where
    display loc = display $ livenessProp loc

type LivenessState = State (RegVariant Liveness)

data LivenessInstKind

instance InstKind LivenessInstKind where
    type InstStateTy LivenessInstKind = LivenessLoc
    type AllowInstBranch LivenessInstKind = True

type LivenessInst = Inst LivenessInstKind
type LivenessCodeBlock = HCodeBlock LivenessInstKind

data RegGraph a = RegGraph
    { vertices :: Set RegID
    , edges :: Map RegID (Set RegID)
    }
    deriving (Show, Eq)

type LivenessGraph = RegVariant RegGraph

toGraph :: [RegVariant Liveness] -> RegVariant RegGraph
toGraph l = RegVariant iGraph fGraph
  where
    iGraph = toGraphEach $ map (#!! RInt) l
    fGraph = toGraphEach $ map (#!! RFloat) l

    toGraphEach :: [Liveness a] -> RegGraph a
    toGraphEach states = RegGraph iV iE
      where
        iState = map alive states
        iV = unions iState
        iE = fromList $ map (\v -> (v, unions (filter (elem v) iState))) (toAscList iV)

markUsed :: Register a -> LivenessState ()
markUsed (Register rTy (SavedReg regId)) = do
    modify $ updateVariant rTy (Liveness . insert regId . alive)
markUsed _ = pure ()

markUsedImm :: RegOrImm a -> LivenessState ()
markUsedImm (Reg reg) = markUsed reg
markUsedImm _ = pure ()

remove :: Register a -> LivenessState ()
remove (Register rTy (SavedReg regId)) = do
    modify $ updateVariant rTy (Liveness . delete regId . alive)
remove _ = pure ()

getState :: Loc -> LivenessState LivenessLoc
getState loc' =
    gets $ LivenessLoc loc'

-- | Calculates liveness information for each instruction.
liveness :: [AbstInst] -> [LivenessInst]
liveness inst = reverse $ evalState (mapM liveness' $ reverse inst) $ RegVariant (Liveness empty) (Liveness empty)
  where
    liveness' :: AbstInst -> LivenessState LivenessInst
    liveness' (ICompOp state op dest src1 src2) = do
        state' <- getState state
        remove dest
        markUsed src1
        markUsedImm src2
        pure $ ICompOp state' op dest src1 src2
    liveness' (IIntOp state op dest src1 src2) = do
        state' <- getState state
        remove dest
        markUsed src1
        markUsedImm src2
        pure $ IIntOp state' op dest src1 src2
    liveness' (IFOp state op dest src1 src2) = do
        state' <- getState state
        remove dest
        markUsed src1
        markUsed src2
        pure $ IFOp state' op dest src1 src2
    liveness' (IMov state dest src) = do
        state' <- getState state
        remove dest
        markUsedImm src
        pure $ IMov state' dest src
    liveness' (IRichCall state label iArgs fArgs) = do
        state' <- getState state
        mapM_ markUsedImm iArgs
        mapM_ markUsed fArgs
        pure $ IRichCall state' label iArgs fArgs
    liveness' (IClosureCall state cl iArgs fArgs) = do
        state' <- getState state
        markUsed cl
        mapM_ markUsedImm iArgs
        mapM_ markUsed fArgs
        pure $ IClosureCall state' cl iArgs fArgs
    liveness' (IMakeClosure state dest label iArgs fArgs) = do
        state' <- getState state
        remove dest
        mapM_ markUsedImm iArgs
        mapM_ markUsed fArgs
        pure $ IMakeClosure state' dest label iArgs fArgs
    liveness' (ILoad state dest src offset) = do
        state' <- getState state
        remove dest
        markUsed src
        pure $ ILoad state' dest src offset
    liveness' (IStore state dest src offset) = do
        state' <- getState state
        markUsed dest
        markUsed src
        pure $ IStore state' dest src offset
    liveness' (IRawInst state name retTy iArgs fArgs) = do
        state' <- getState state
        remove retTy
        mapM_ markUsedImm iArgs
        mapM_ markUsed fArgs
        pure $ IRawInst state' name retTy iArgs fArgs
