{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module BackEnd.Liveness (
    Liveness (..),
    LivenessLoc (..),
    LivenessInst,
    LivenessBlock,
    LivenessGraph,
    LivenessInstKind,
    constructGraph,
    allLivenessInfo,
) where

import BackEnd.Algorithm.Graph (RegGraph (RegGraph))
import CodeBlock (BlockGraph, CodeBlock, visitBlock, visitInst)
import Control.Monad.State (execState, modify)
import Data.Map (fromList)
import Data.Set (Set, empty, toAscList, union, unions)
import Data.Text (intercalate)
import Display (Display (display))
import IR (
    Inst (..),
    InstKind (..),
    getIState,
 )
import Registers (
    RegID,
    RegMultiple,
    RegType (RFloat, RInt),
    Register (Register),
    RegisterKind (SavedReg),
    buildRT,
    (#!!),
 )
import Syntax (Loc)
import Prelude hiding (lookup)

newtype Liveness = Liveness
    { alive :: Set RegID
    }
    deriving (Show, Eq)

instance Semigroup Liveness where
    Liveness a <> Liveness b = Liveness $ a `union` b

instance Monoid Liveness where
    mempty = Liveness empty

instance Display (RegMultiple Liveness) where
    display live =
        "["
            <> intercalate "," (map (display . Register RInt . SavedReg) (toAscList (alive $ live #!! RInt)))
            <> "], ["
            <> intercalate "," (map (display . Register RFloat . SavedReg) (toAscList (alive $ live #!! RFloat)))
            <> "]"

data LivenessLoc = LivenessLoc
    { livenessLoc :: Loc
    , livenessProp :: RegMultiple Liveness
    }
    deriving (Show, Eq)

instance Display LivenessLoc where
    display loc = display $ livenessProp loc

data LivenessInstKind

instance InstKind LivenessInstKind where
    type InstStateTy LivenessInstKind = LivenessLoc
    type AllowPhi LivenessInstKind = True

type LivenessInst = Inst LivenessInstKind
type LivenessBlock = CodeBlock LivenessInstKind
type LivenessGraph = BlockGraph LivenessInstKind

-- | Constructs a registers graph from a list of liveness information.
constructGraph :: [RegMultiple Liveness] -> RegMultiple RegGraph
constructGraph l = buildRT (\rTy -> toGraphEach (map (#!! rTy) l))
  where
    toGraphEach :: [Liveness] -> RegGraph
    toGraphEach states = RegGraph vertices' edges'
      where
        aliveSet = map alive states
        vertices' = unions aliveSet
        edges' = fromList $ map (\v -> (v, unions (filter (elem v) aliveSet))) (toAscList vertices')

-- | Gets liveness information from the code block.
allLivenessInfo :: LivenessGraph -> [RegMultiple Liveness]
allLivenessInfo graph =
    execState
        ( visitBlock
            ( visitInst
                ( \inst -> do
                    modify $ \ctx -> livenessProp (getIState inst) : ctx
                    pure inst
                )
            )
            graph
        )
        []
