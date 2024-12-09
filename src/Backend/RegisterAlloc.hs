{-# LANGUAGE GADTs #-}

module Backend.RegisterAlloc (assignRegister) where

import Backend.Asm (
    AllowBranch,
    Inst,
    IntermediateCodeBlock (getICBInst),
    Register (SavedReg, TempReg),
    getAllIState,
    replaceFReg,
    replaceIReg,
    substIState,
 )
import Backend.BackendEnv (BackendStateT, RegID)
import Backend.Liveness (LivenessGraph (LivenessGraph), LivenessLoc (livenessLoc, livenessState), RegGraph (RegGraph, edges), toGraph)
import Control.Monad.State (State, execState, gets, modify)
import Data.List (sortBy)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (Down), comparing)
import Syntax (Loc)

newtype RegAllocEnv = RegAllocEnv
    { regMap :: [(RegID, RegID)]
    }
    deriving (Show, Eq)

type RegAllocState = State RegAllocEnv

mapAll :: [RegID] -> RegAllocState [RegID]
mapAll registers = do
    regMap' <- gets regMap
    pure $ mapMaybe (`lookup` regMap') registers

assign :: RegID -> RegID -> RegAllocState ()
assign reg1 reg2 = do
    modify $ \env -> env{regMap = (reg1, reg2) : regMap env}

getMin :: [RegID] -> RegID
getMin l = until (`notElem` l) (+ 1) 0

sortByDegree :: RegGraph -> [RegID]
sortByDegree (RegGraph vertices edges') =
    map (negate . snd)
        $ sortBy
            (comparing Down)
        $ map (\i -> (length $ lookup i edges', -i)) vertices

selectSpilt :: RegGraph -> Maybe RegID
selectSpilt = listToMaybe . sortByDegree

registerAlloc :: LivenessGraph -> ([(RegID, RegID)], [(RegID, RegID)])
registerAlloc (LivenessGraph iGraph fGraph) = (iGraph', fGraph')
  where
    iGraph' = regMap $ execState (registerAlloc' iGraph) (RegAllocEnv [])
    fGraph' = regMap $ execState (registerAlloc' fGraph) (RegAllocEnv [])

    registerAlloc' :: RegGraph -> RegAllocState ()
    registerAlloc' graph =
        mapM_
            ( \reg -> do
                let neighborhood = map snd $ filter ((== reg) . fst) $ edges graph
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
    let usedI = (+ 1) $ foldl max (-1) $ map snd iMap
    let usedF = (+ 1) $ foldl max (-1) $ map snd fMap

    let iSpillTarget = selectSpilt iGraph
    let fSpillTarget = selectSpilt fGraph

    -- Accept the register allocation.
    -- Replace SavedReg with TempReg.
    let inst' = map (\i -> foldl (\i' (a, b) -> replaceIReg (SavedReg a) (TempReg b) i') i iMap) inst
    let inst'' = map (\i -> foldl (\i' (a, b) -> replaceFReg (SavedReg a) (TempReg b) i') i fMap) inst'

    -- To avoid the replacement of SavedReg occurring more than once,
    -- we need to proceed replacements via TempReg.
    let inst''' = map (\i -> foldl (\i' a -> replaceIReg (TempReg a) (SavedReg a) i') i [0 .. usedI - 1]) inst''
    let inst'''' = map (\i -> foldl (\i' a -> replaceFReg (TempReg a) (SavedReg a) i') i [0 .. usedF - 1]) inst'''

    pure (usedI, usedF, iSpillTarget, fSpillTarget, block{getICBInst = map (substIState livenessLoc) inst''''})
  where
    retrieveGraph :: [Inst LivenessLoc RegID AllowBranch] -> LivenessGraph
    retrieveGraph inst' = toGraph $ concatMap (map livenessState . getAllIState) inst'

    inst :: [Inst LivenessLoc RegID AllowBranch]
    inst = getICBInst block
