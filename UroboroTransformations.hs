module UroboroTransformations where

import Data.Maybe

import Uroboro.Tree

import qualified UroboroTransformations.MixedDefs.Defunc as MixedDefsD
import qualified UroboroTransformations.Entangled.Defunc as EntangledD

import UroboroTransformations.Util

import Data.List(groupBy)

helperFunRule :: Identifier -> PTRule -> PTRule
helperFunRule id (PTRule l (PQDes l' id' pps pq@(PQDes l'' _ _ _)) e) =
    PTRule l (PQDes l' id' pps (PQApp l'' id (collectVarsPQ pq))) e
helperFunRule _ _ = undefined

helperFun :: [PT] -> (PT, [PTRule]) -> PT
helperFun pts ((PTFun _ _ ts t _), rs@((PTRule l (PQDes _ _ _ pq@(PQDes _ des _ _)) _):_)) =
    PTFun l helperFunId (collectVarTypes pts ts pq) (destructorReturnType des pts) (map (helperFunRule helperFunId) rs)
  where
    helperFunId = gensym "extract" (namePattern pq) pts
helperFun _ _ = undefined

helperFuns :: [PT] -> [PT]
helperFuns pts = map (helperFun pts) $ concatMap outerDesRulesGroupedByPattern $ filter isFun pts
  where
    outerDesRulesGroupedByPattern fun@(PTFun _ _ _ _ rs) =
        map ((,) fun) $ groupBy hasSamePatternAs $ filter isOuterDesRule rs

    isOuterDesRule (PTRule _ (PQDes _ _ _ (PQDes _ _ _ _)) _) = True
    isOuterDesRule _ = False

    hasSamePatternAs :: PTRule -> PTRule -> Bool
    (PTRule _ (PQDes _ _ _ pq) _) `hasSamePatternAs` (PTRule _ (PQDes _ _ _ pq') _) =
        pq `isSamePatternAs` pq'
    _ `hasSamePatternAs` _ = False

    isSamePatternAs :: PQ -> PQ -> Bool
    (PQApp _ _ pps) `isSamePatternAs` (PQApp _ _ pps') = and $ zipWith isSamePPPatternAs pps pps'
    (PQDes _ _ pps pq) `isSamePatternAs` (PQDes _ _ pps' pq') =
        (and $ zipWith isSamePPPatternAs pps pps') && (pq `isSamePatternAs` pq')

    isSamePPPatternAs :: PP -> PP -> Bool
    (PPVar _ _) `isSamePPPatternAs` (PPVar _ _) = True
    (PPCon _ id pps) `isSamePPPatternAs` (PPCon _ id' pps') =
        (id == id') && (and $ zipWith isSamePPPatternAs pps pps')
    _ `isSamePPPatternAs` _ = False

    isFun (PTFun _ _ _ _ _) = True
    isFun _ = False

containsMultiDes :: PT -> Bool
containsMultiDes (PTFun _ _ _ _ rs) = any ruleContainsMultiDes rs
  where
    ruleContainsMultiDes (PTRule _ (PQDes _ _ _ (PQDes _ _ _ _)) _) = True
    ruleContainsMultiDes _ = False
containsMultiDes _ = False

extractOuterDesCallsInRule :: [PT] -> PTRule -> PTRule
extractOuterDesCallsInRule pts r@(PTRule l (PQDes _ _ _ pq@(PQDes l' _ _ _)) _e) =
    PTRule l pq (PApp l' (gensym "extract" (namePattern pq) pts) (map toExpr $ collectVarsPQ pq))
extractOuterDesCallsInRule _ r = r

extractOuterDesCalls :: [PT] -> PT -> PT
extractOuterDesCalls pts (PTFun l id ts t rs) = PTFun l id ts t (map (extractOuterDesCallsInRule pts) rs)
extractOuterDesCalls _ pt = pt

extractOuterDes :: [PT] -> [PT]
extractOuterDes pts = (map (extractOuterDesCalls pts) pts) ++ (helperFuns pts)

elimMultiDes :: [PT] -> [PT]
elimMultiDes pts
    | any containsMultiDes pts = elimMultiDes $ extractOuterDes pts
    | otherwise                = pts

-- |Defunctionalize an Uroboro program
defunc :: [PT] -> Maybe [PT]
defunc pts = EntangledD.defuncLegal $ MixedDefsD.desExtract $ elimMultiDes pts