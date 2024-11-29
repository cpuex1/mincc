{-# LANGUAGE GADTs #-}

module Backend.RegisterAlloc (assignRegister) where

import Backend.Asm (
    AllowBranch,
    Inst,
    IntermediateCodeBlock (IntermediateCodeBlock),
    Register (SavedReg, TempReg),
    getAllIState,
    replaceFReg,
    replaceIReg,
    substIState,
 )
import Backend.BackendEnv (BackendEnv (generatedFReg, generatedIReg, usedFRegLen, usedIRegLen), BackendStateT, RegID)
import Backend.Liveness (LivenessGraph (LivenessGraph), LivenessLoc (livenessLoc, livenessState), toGraph)
import Control.Monad.State (State, execState, gets, modify)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
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

sortByDegree :: Int -> [(RegID, RegID)] -> [RegID]
sortByDegree maxID graph =
    map fst $ sortOn (negate . snd) $ map (\i -> (i, length $ lookup i graph)) [0 .. maxID - 1]

registerAlloc :: Int -> Int -> LivenessGraph -> ([(RegID, RegID)], [(RegID, RegID)])
registerAlloc iMaxID fMaxID (LivenessGraph iGraph fGraph) = (iGraph', fGraph')
  where
    iGraph' = regMap $ execState (registerAlloc' iMaxID iGraph) (RegAllocEnv [])
    fGraph' = regMap $ execState (registerAlloc' fMaxID fGraph) (RegAllocEnv [])

    registerAlloc' :: Int -> [(RegID, RegID)] -> RegAllocState ()
    registerAlloc' maxID graph =
        mapM_
            ( \reg -> do
                let neighborhood = map snd $ filter ((== reg) . fst) graph
                mapped <- mapAll neighborhood
                let minReg = getMin mapped
                assign reg minReg
            )
            sorted
      where
        sorted = sortByDegree maxID graph

assignRegister :: (Monad m) => IntermediateCodeBlock LivenessLoc RegID -> BackendStateT m (IntermediateCodeBlock Loc RegID)
assignRegister (IntermediateCodeBlock label prologue inst epilogue) = do
    iMaxID <- gets generatedIReg
    fMaxID <- gets generatedFReg
    let (iMap, fMap) = registerAlloc iMaxID fMaxID $ retrieveGraph inst
    let inst' = map (\i -> foldl (\i' (a, b) -> replaceIReg (TempReg a) (SavedReg b) i') i iMap) inst
    let inst'' = map (\i -> foldl (\i' (a, b) -> replaceFReg (TempReg a) (SavedReg b) i') i fMap) inst'
    modify $ \env ->
        env
            { usedIRegLen = max (usedIRegLen env) $ (+ 1) $ foldl max (-1) $ map snd iMap
            , usedFRegLen = max (usedFRegLen env) $ (+ 1) $ foldl max (-1) $ map snd fMap
            }
    pure $
        IntermediateCodeBlock
            label
            (map (substIState livenessLoc) prologue)
            (map (substIState livenessLoc) inst'')
            (map (substIState livenessLoc) epilogue)
  where
    retrieveGraph :: [Inst LivenessLoc RegID AllowBranch] -> LivenessGraph
    retrieveGraph inst' = toGraph $ concatMap (map livenessState . getAllIState) inst'
