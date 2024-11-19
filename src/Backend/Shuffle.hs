module Backend.Shuffle (
    runDetectLoop,
    detectAllLoops,
    genCirc,
    shuffle,
    shuffleRegs,
) where

import Backend.Asm (RegID, Register (ArgsReg, TempReg))
import Control.Monad (when)
import Control.Monad.State (State, evalState, gets, modify)
import Data.Map.Lazy (Map, alter, empty, insert, lookup)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (difference, fromList, toList)
import Prelude hiding (lookup)

newtype RegGraph a = RegGraph
    { visited :: Map a [a]
    }
    deriving (Show, Eq)

runDetectLoop :: (Ord a) => a -> [(a, a)] -> Maybe [a]
runDetectLoop v' edges = evalState (detectLoop v' edges') (RegGraph empty)
  where
    edges' =
        foldl
            ( \e (v1, v2) ->
                alter ((\l -> Just $ l ++ [v2]) . fromMaybe []) v1 e
            )
            empty
            edges

    detectLoop :: (Ord a) => a -> Map a [a] -> State (RegGraph a) (Maybe [a])
    detectLoop start edges'' = do
        visit start edges''
        visited' <- gets visited
        let path = fromMaybe [] $ lookup start visited'
        pure $ if null path then Nothing else Just path
      where
        visit :: (Ord a) => a -> Map a [a] -> State (RegGraph a) ()
        visit v edges''' = do
            visited' <- gets visited
            let neighbors = fromMaybe [] $ lookup v edges'''
            let path = fromMaybe [] $ lookup v visited'
            mapM_
                ( \n -> do
                    visited'' <- gets visited
                    when (null $ fromMaybe [] $ lookup n visited'') $ do
                        modify $ \graph ->
                            graph{visited = insert n (n : path) (visited graph)}
                        visit n edges'''
                )
                neighbors

detectAllLoops :: (Ord a) => [a] -> [(a, a)] -> [[a]]
detectAllLoops [] _ = []
detectAllLoops (a : args) assign =
    case runDetectLoop a assign of
        Just loop ->
            let args' = toList $ difference (fromList args) (fromList loop)
             in loop : detectAllLoops args' assign
        Nothing ->
            detectAllLoops args assign

genCirc :: a -> [a] -> [(a, a)]
genCirc temp regs = do
    reverse $
        map (\i -> ((temp : regs) !! (i + 1), (temp : regs) !! i)) [0 .. length regs - 1]
            ++ [(temp, regs !! (length regs - 1))]

shuffle :: (Ord a) => a -> [(a, a)] -> [(a, a)]
shuffle temp assign =
    filter (\a -> fst a `elem` simple_args) assign'
        ++ concatMap (genCirc temp) loops
  where
    assign' = filter (uncurry (/=)) assign
    args' = map fst assign'
    loops = detectAllLoops args' assign
    resolve_in_loops = concat loops
    simple_args = toList $ difference (fromList args') (fromList resolve_in_loops)

shuffleRegs :: [(Register RegID a, Register RegID a)] -> [(Register RegID a, Register RegID a)]
shuffleRegs assign =
    map
        ( \(r1, r2) ->
            let r1' = if r1 == tempRegID then TempReg 0 else ArgsReg r1
             in let r2' = if r2 == tempRegID then TempReg 0 else ArgsReg r2
                 in (r1', r2')
        )
        (shuffle tempRegID assignArgs)
        ++ overwrittenAssign
  where
    tempRegID = -1

    assignArgs =
        mapMaybe
            ( \(r1, r2) ->
                case (r1, r2) of
                    (ArgsReg i1, ArgsReg i2) -> Just (i1, i2)
                    _ -> Nothing
            )
            assign
    overwrittenAssign =
        filter
            ( \(r1, r2) ->
                case (r1, r2) of
                    (ArgsReg _, ArgsReg _) -> False
                    _ -> True
            )
            assign
