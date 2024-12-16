{-# LANGUAGE GADTs #-}

module Backend.RegisterAlloc (assignRegister) where

import Backend.Asm (
    AllowBranch,
    Inst,
    IntermediateCodeBlock (getICBInst),
    Register (SavedReg, ZeroReg),
    getAllIState,
    replaceFReg,
    replaceIReg,
    substIState,
 )
import Backend.BackendEnv (BackendEnv (generatedFReg, generatedIReg), BackendStateT, RegID)
import Backend.Liveness (LivenessGraph (LivenessGraph), LivenessLoc (livenessLoc, livenessState), RegGraph (RegGraph, edges), toGraph)
import Control.Monad.State (State, execState, gets, modify)
import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Set (Set, notMember, size, toDescList)
import qualified Data.Set as S
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
    genI <- gets generatedIReg
    genF <- gets generatedFReg

    let iSpillTarget = selectSpilt iGraph
    let fSpillTarget = selectSpilt fGraph

    -- Accept the register allocation.
    -- Replace SavedReg with TempReg.
    let inst' = map (\i -> foldl (\i' (a, b) -> replaceIReg (SavedReg a) (SavedReg (-b - 1)) i') i $ M.toList iMap) inst
    let inst'' = map (\i -> foldl (\i' (a, b) -> replaceFReg (SavedReg a) (SavedReg (-b - 1)) i') i $ M.toList fMap) inst'

    -- Ground unallocated registers.
    let inst''' = map (\i -> foldl (\i' a -> replaceIReg (SavedReg a) ZeroReg i') i [0 .. genI - 1]) inst''
    let inst'''' = map (\i -> foldl (\i' a -> replaceFReg (SavedReg a) ZeroReg i') i [0 .. genF - 1]) inst'''

    -- To avoid the replacement of SavedReg occurring more than once,
    -- we need to proceed replacements via negative register IDs.
    -- Do not forget s0 and fs0, which have non-positive ones.
    let inst''''' = map (\i -> foldl (\i' a -> replaceIReg (SavedReg (-a - 1)) (SavedReg a) i') i [0 .. usedI - 1]) inst''''
    let inst'''''' = map (\i -> foldl (\i' a -> replaceFReg (SavedReg (-a - 1)) (SavedReg a) i') i [0 .. usedF - 1]) inst'''''

    pure (usedI, usedF, iSpillTarget, fSpillTarget, block{getICBInst = map (substIState livenessLoc) inst''''''})
  where
    retrieveGraph :: [Inst LivenessLoc RegID AllowBranch] -> LivenessGraph
    retrieveGraph inst' = toGraph $ concatMap (map livenessState . getAllIState) inst'

    inst :: [Inst LivenessLoc RegID AllowBranch]
    inst = getICBInst block
