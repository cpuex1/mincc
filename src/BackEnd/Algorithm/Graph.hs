module BackEnd.Algorithm.Graph (
    RegGraph (..),
    coloringByDegree,
    minimumUnused,
    sortByDegree,
) where

import Data.Map (Map, findWithDefault, insert, lookup)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set, fromList, notMember, size, toDescList, toList)
import qualified Data.Set as S
import Registers (RegID, RegMapping (RegMapping))
import Prelude hiding (lookup)

data RegGraph = RegGraph
    { vertices :: Set RegID
    , edges :: Map RegID (Set RegID)
    }
    deriving (Show, Eq)

-- | Sorts vertices with degree.
sortByDegree :: RegGraph -> [RegID]
sortByDegree (RegGraph vertices' edges') =
    map snd $ toDescList degreeSet
  where
    degreeMap = M.map size edges'
    degreeSet = S.map (\v -> (findWithDefault 0 v degreeMap, v)) vertices'

{- | Gets the minimum register ID that is not used.
Since register IDs has gaps, we can't just use the maximum register ID.
-}
minimumUnused :: Set RegID -> RegID
minimumUnused l = until (`notMember` l) (+ 1) 0

-- | Performs graph coloring with degrees.
coloringByDegree :: RegGraph -> RegMapping
coloringByDegree graph = RegMapping $ foldl coloringVertex mempty sorted
  where
    sorted = sortByDegree graph

    coloringVertex :: Map RegID RegID -> RegID -> Map RegID RegID
    coloringVertex mapping v =
        insert v color mapping
      where
        neighborhood = toList $ findWithDefault S.empty v $ edges graph
        usedColors = fromList $ mapMaybe (`lookup` mapping) neighborhood
        color = minimumUnused usedColors
