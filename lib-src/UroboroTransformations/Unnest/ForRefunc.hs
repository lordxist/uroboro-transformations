module UroboroTransformations.Unnest.ForRefunc where

import UroboroTransformations.Unnest
import UroboroTransformations.Util

import Uroboro.Tree

import Data.List

isRNestedFunDef :: PT -> Bool
isRNestedFunDef (PTFun _ _ _ _ rs) = any isRNestedRule rs
  where
    isRNestedRule (PTRule _ pq _) = isRNestedLhs pq

    isRNestedLhs (PQApp _ _ pps) = (length $ findIndices con pps) > 1
    isRNestedLhs pq = any con (ppsInPQ pq)

    ppsInPQ (PQApp _ _ pps) = pps
    ppsInPQ (PQDes _ _ pps pq) = pps ++ (ppsInPQ pq)
isRNestedFunDef _ = False

unnestForRefunc :: [PT] -> Maybe [PT]
unnestForRefunc = unnestFor isRNestedFunDef
