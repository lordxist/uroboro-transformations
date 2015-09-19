module UroboroTransformations.CopatternCoverage (checkCoverage) where

import Control.Monad.Reader
import Data.Maybe
import Data.List (find)
import Data.Set (fromList)

import Uroboro.Tree
import Uroboro.Checker

import UroboroTransformations.CopatternCoverage.CCTree
import UroboroTransformations.Util
import UroboroTransformations.Util.Conversion
import UroboroTransformations.Util.UroboroEnhanced

leavesEqualPQs :: [PQ] -> CCTree -> Bool
leavesEqualPQs pqs tree = (fromList (map tqToPQ (leaves tree))) == (fromList pqs)

-- |Tries to derive the coverage tree for the given function definition.
checkCoverage :: PTSig -> PT -> Reader Program (Maybe CCTree)
checkCoverage sig (PTFun _ _ _ _ rs) = (liftM $ find (leavesEqualPQs (map lhs rs))) searchSpace
  where
    searchSpace = possibleTrees sig (maximum $ map (splittingDepth . lhs) rs)

    lhs (PTRule _ pq _) = pq
