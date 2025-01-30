{-# LANGUAGE GADTs #-}

module BackEnd.RegisterAlloc (assignRegister) where

import BackEnd.Liveness (LivenessLoc (livenessLoc, livenessProp), RegGraph (RegGraph, edges), toGraph)
import Control.Monad.State (State, execState, gets, modify)
import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Set (Set, notMember, size, toDescList)
import qualified Data.Set as S
import IR (
    IntermediateCodeBlock (getICBInst),
    RegID,
    getAllIState,
    mapReg,
    substIState,
 )
import Registers (
    RegVariant',
    Register (Register),
    RegisterKind (SavedReg),
    VariantItem (VariantItem),
    mapVariant,
    savedReg,
    selectVariant,
    zeroReg,
 )
import Syntax (Loc)

newtype RegAllocEnv a = RegAllocEnv
    { regMap :: Map RegID RegID
    }
    deriving (Show, Eq)

type RegAllocState a = State (RegAllocEnv a)

mapAll :: Set RegID -> RegAllocState a (Set (Maybe RegID))
mapAll registers = do
    regMap' <- gets regMap
    pure $ S.map (`M.lookup` regMap') registers

assign :: RegID -> RegID -> RegAllocState a ()
assign reg1 reg2 = do
    modify $ \env -> env{regMap = M.insert reg1 reg2 $ regMap env}

getMin :: Set (Maybe RegID) -> RegID
getMin l = until ((`notMember` l) . Just) (+ 1) 0

sortByDegree :: RegGraph a -> [RegID]
sortByDegree (RegGraph vertices edges') =
    map snd $ toDescList degreeSet
  where
    degreeMap = M.map size edges'
    degreeSet = S.map (\v -> (findWithDefault 0 v degreeMap, v)) vertices

selectSpilt :: RegGraph a -> VariantItem (Maybe RegID) a
selectSpilt = VariantItem . listToMaybe . sortByDegree

runRegisterAlloc :: RegGraph a -> RegAllocEnv a
runRegisterAlloc graph = execState (registerAlloc graph) (RegAllocEnv M.empty)
  where
    registerAlloc :: RegGraph a -> RegAllocState a ()
    registerAlloc graph' =
        mapM_
            ( \reg -> do
                let neighborhood = findWithDefault S.empty reg $ edges graph'
                mapped <- mapAll neighborhood
                let minReg = getMin mapped
                assign reg minReg
            )
            sorted
      where
        sorted = sortByDegree graph'

assignRegister :: IntermediateCodeBlock LivenessLoc RegID -> (RegVariant' Int, RegVariant' (Maybe RegID), IntermediateCodeBlock Loc RegID)
assignRegister block =
    -- Accept the register allocation.
    (used, spillTarget, block{getICBInst = map (substIState livenessLoc) mappedInst})
  where
    enforceMapped :: Register RegID a -> Register RegID a
    enforceMapped (Register rTy (SavedReg regId)) =
        case M.lookup regId $ regMap $ selectVariant rTy mapped of
            Just regId' -> savedReg rTy regId'
            Nothing -> zeroReg rTy
    enforceMapped reg = reg

    inst = getICBInst block
    graph = toGraph $ concatMap (map livenessProp . getAllIState) inst
    mapped = mapVariant runRegisterAlloc graph
    used = mapVariant (VariantItem . (+ 1) . foldl max (-1) . M.elems . regMap) mapped
    spillTarget = mapVariant selectSpilt graph
    mappedInst = map (mapReg enforceMapped) inst
