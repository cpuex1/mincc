{-# LANGUAGE GADTs #-}

module BackEnd.RegisterAlloc (assignRegister) where

import BackEnd.BackendEnv (BackendStateT)
import BackEnd.Liveness (LivenessGraph (LivenessGraph), LivenessLoc (livenessLoc, livenessState), RegGraph (RegGraph, edges), toGraph)
import Control.Monad.State (State, execState, gets, modify)
import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Set (Set, notMember, size, toDescList)
import qualified Data.Set as S
import IR (
    AllowBranch,
    Inst,
    IntermediateCodeBlock (getICBInst),
    RegID,
    getAllIState,
    mapReg,
    substIState,
 )
import Registers (RegType (RFloat, RInt), Register (Register), RegisterKind (SavedReg), savedReg, zeroReg)
import Syntax (Loc)

newtype RegAllocEnv = RegAllocEnv
    { regMap :: Map RegID RegID
    }
    deriving (Show, Eq)

type RegAllocState = State RegAllocEnv

mapAll :: Set RegID -> RegAllocState (Set (Maybe RegID))
mapAll registers = do
    regMap' <- gets regMap
    pure $ S.map (`M.lookup` regMap') registers

assign :: RegID -> RegID -> RegAllocState ()
assign reg1 reg2 = do
    modify $ \env -> env{regMap = M.insert reg1 reg2 $ regMap env}

getMin :: Set (Maybe RegID) -> RegID
getMin l = until ((`notMember` l) . Just) (+ 1) 0

sortByDegree :: RegGraph -> [RegID]
sortByDegree (RegGraph vertices edges') =
    map snd $ toDescList degreeSet
  where
    degreeMap = M.map size edges'
    degreeSet = S.map (\v -> (findWithDefault 0 v degreeMap, v)) vertices

selectSpilt :: RegGraph -> Maybe RegID
selectSpilt = listToMaybe . sortByDegree

registerAlloc :: LivenessGraph -> (Map RegID RegID, Map RegID RegID)
registerAlloc (LivenessGraph iGraph fGraph) = (iGraph', fGraph')
  where
    iGraph' = regMap $ execState (registerAlloc' iGraph) (RegAllocEnv M.empty)
    fGraph' = regMap $ execState (registerAlloc' fGraph) (RegAllocEnv M.empty)

    registerAlloc' :: RegGraph -> RegAllocState ()
    registerAlloc' graph =
        mapM_
            ( \reg -> do
                let neighborhood = findWithDefault S.empty reg $ edges graph
                mapped <- mapAll neighborhood
                let minReg = getMin mapped
                assign reg minReg
            )
            sorted
      where
        sorted = sortByDegree graph

assignRegister :: (Monad m) => IntermediateCodeBlock LivenessLoc RegID -> BackendStateT m (Int, Int, Maybe RegID, Maybe RegID, IntermediateCodeBlock Loc RegID)
assignRegister block = do
    let (LivenessGraph iGraph fGraph) = retrieveGraph inst
    let (iMap, fMap) = registerAlloc (LivenessGraph iGraph fGraph)
    let usedI = (+ 1) $ foldl max (-1) $ M.elems iMap
    let usedF = (+ 1) $ foldl max (-1) $ M.elems fMap

    let iSpillTarget = selectSpilt iGraph
    let fSpillTarget = selectSpilt fGraph

    -- Accept the register allocation.
    let inst' =
            map
                ( mapReg
                    ( \reg -> case reg of
                        Register RInt (SavedReg regId) ->
                            case M.lookup regId iMap of
                                Just regId' -> savedReg RInt regId'
                                Nothing -> zeroReg RInt
                        Register RFloat (SavedReg regId) ->
                            case M.lookup regId fMap of
                                Just regId' -> savedReg RFloat regId'
                                Nothing -> zeroReg RFloat
                        _ -> reg
                    )
                )
                inst

    pure (usedI, usedF, iSpillTarget, fSpillTarget, block{getICBInst = map (substIState livenessLoc) inst'})
  where
    retrieveGraph :: [Inst LivenessLoc RegID AllowBranch] -> LivenessGraph
    retrieveGraph inst' = toGraph $ concatMap (map livenessState . getAllIState) inst'

    inst :: [Inst LivenessLoc RegID AllowBranch]
    inst = getICBInst block
