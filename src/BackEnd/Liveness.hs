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
    LivenessBlockGraph,
    LivenessInstKind,
    toGraph,
) where

import CodeBlock (BlockGraph, CodeBlock)
import Data.Map (Map, fromList)
import Data.Set (Set, empty, toAscList, union, unions)
import Data.Text (intercalate)
import Display (Display (display))
import IR (
    AbstInst,
    Inst (..),
    InstKind (..),
 )
import Registers (
    RegID,
    RegType (RFloat, RInt),
    RegVariant (RegVariant),
    Register (Register),
    RegisterKind (SavedReg),
    (#!!),
 )
import Syntax (Loc)
import Prelude hiding (lookup)

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

data LivenessInstKind

instance InstKind LivenessInstKind where
    type InstStateTy LivenessInstKind = LivenessLoc
    type AllowPseudoCall LivenessInstKind = True
    type AllowPhi LivenessInstKind = True

type LivenessInst = Inst LivenessInstKind
type LivenessCodeBlock = CodeBlock LivenessInstKind
type LivenessBlockGraph = BlockGraph LivenessInstKind

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

-- | Calculates liveness information for each instruction.
liveness :: [AbstInst] -> [LivenessInst]
liveness _ = undefined

-- reverse $ evalState (mapM instLiveness $ reverse inst) $ RegVariant (Liveness empty) (Liveness empty)
