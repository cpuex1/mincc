{-# LANGUAGE GADTs #-}

module BackEnd.RegisterAlloc (assignRegister) where

import BackEnd.Liveness (LivenessInstKind, LivenessLoc (livenessLoc, livenessProp), RegGraph (RegGraph, edges), toGraph)
import Control.Monad.State (State, execState, gets, modify)
import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Set (Set, notMember, size, toDescList)
import qualified Data.Set as S
import IR (
    AbstCodeBlock,
    HCodeBlock (hInst),
    getIState,
    mapReg,
    substIState,
 )
import Registers (
    RegID,
    RegVariant',
    Register (Register),
    RegisterKind (SavedReg),
    VariantItem (VariantItem),
    savedReg,
    zeroReg,
    (#!!),
    (#$),
 )

-- | Holds register mapping.
newtype RegAllocEnv a = RegAllocEnv
    { regMap :: Map RegID RegID
    }
    deriving (Show, Eq)

type RegAllocState a = State (RegAllocEnv a)

-- | Maps all registers to their assigned registers.
mapAll :: Set RegID -> RegAllocState a (Set (Maybe RegID))
mapAll registers = do
    regMap' <- gets regMap
    pure $ S.map (`M.lookup` regMap') registers

-- | Assigns a register to another register.
assign :: RegID -> RegID -> RegAllocState a ()
assign reg1 reg2 = do
    modify $ \env -> env{regMap = M.insert reg1 reg2 $ regMap env}

{- | Gets the minimum register ID that is not used.
Since register IDs has gaps, we can't just use the maximum register ID.
-}
getMin :: Set (Maybe RegID) -> RegID
getMin l = until ((`notMember` l) . Just) (+ 1) 0

-- | Sorts vertices with degree.
sortByDegree :: RegGraph a -> [RegID]
sortByDegree (RegGraph vertices edges') =
    map snd $ toDescList degreeSet
  where
    degreeMap = M.map size edges'
    degreeSet = S.map (\v -> (findWithDefault 0 v degreeMap, v)) vertices

-- | Selects a target to be spilt.
selectSpilt :: RegGraph a -> VariantItem (Maybe RegID) a
selectSpilt = VariantItem . listToMaybe . sortByDegree

-- | Runs the register allocation algorithm.
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

-- | Allocates registers.
assignRegister :: HCodeBlock LivenessInstKind -> (RegVariant' Int, RegVariant' (Maybe RegID), AbstCodeBlock)
assignRegister block =
    -- Accept the register allocation.
    (used, spillTarget, block{hInst = map (substIState livenessLoc) mappedInst})
  where
    -- Enforces the register mapping.
    enforceMapped :: Register a -> Register a
    enforceMapped (Register rTy (SavedReg regId)) =
        case M.lookup regId $ regMap $ mapped #!! rTy of
            Just regId' -> savedReg rTy regId'
            Nothing -> zeroReg rTy
    enforceMapped reg = reg

    inst = hInst block
    graph = toGraph $ map (livenessProp . getIState) inst
    mapped = const runRegisterAlloc #$ graph
    used = const (VariantItem . (+ 1) . foldl max (-1) . M.elems . regMap) #$ mapped
    spillTarget = const selectSpilt #$ graph
    mappedInst = map (mapReg enforceMapped) inst
