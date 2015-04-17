module UroboroTransformations.MixedDefs.Defunc where

import Uroboro.Tree

import UroboroTransformations.Util
import qualified UroboroTransformations.Entangled.Defunc as EntangledD
import qualified UroboroTransformations.Entangled.FragmentTest as EntangledTest

import UroboroTransformations.MixedDefs.FragmentTest

import Data.List(groupBy, intercalate)

isMixedRules :: PT -> Bool
isMixedRules (PTFun _ _ _ _ rs) =
    (any EntangledTest.illegalDesPatternRule rs) && (any EntangledTest.illegalHolePatternRule rs)
isMixedRules _ = False

helperFunRule :: Identifier -> PTRule -> PTRule
helperFunRule id (PTRule l (PQDes l' id' pps (PQApp l'' _ pps')) e) =
    PTRule l (PQDes l' id' pps (PQApp l'' id (concatMap collectVars pps'))) e
helperFunRule _ _ = undefined

helperFun :: [PT] -> (PT, [PTRule]) -> PT
helperFun pts ((PTFun _ _ ts t _), rs@((PTRule l (PQDes _ _ _ pq) _):_)) =
    PTFun l helperFunId (collectVarTypes pts ts pq) t (map (helperFunRule helperFunId) rs)
  where
    helperFunId = gensym "extract" (namePattern pq) pts
helperFun _ _ = undefined

helperFuns :: [PT] -> [PT]
helperFuns pts = map (helperFun pts) $ concatMap desRulesGroupedByPattern $ filter isMixedRules pts
  where
    desRulesGroupedByPattern fun@(PTFun _ _ _ _ rs) =
        map ((,) fun) $ groupBy hasSamePatternAs $ filter isDesRule rs

    isDesRule (PTRule _ (PQDes _ _ _ _) _) = True
    isDesRule _ = False

    hasSamePatternAs :: PTRule -> PTRule -> Bool
    (PTRule _ (PQDes _ _ _ pq) _) `hasSamePatternAs` (PTRule _ (PQDes _ _ _ pq') _) =
        pq `isSamePatternAs` pq'
    _ `hasSamePatternAs` _ = False

    isSamePatternAs :: PQ -> PQ -> Bool
    (PQApp _ _ pps) `isSamePatternAs` (PQApp _ _ pps') = and $ zipWith isSamePPPatternAs pps pps'

    isSamePPPatternAs :: PP -> PP -> Bool
    (PPVar _ _) `isSamePPPatternAs` (PPVar _ _) = True
    (PPCon _ id pps) `isSamePPPatternAs` (PPCon _ id' pps') =
        (id == id') && (and $ zipWith isSamePPPatternAs pps pps')
    _ `isSamePPPatternAs` _ = False

extractDesCallsInRule :: [PT] -> PTRule -> PTRule
extractDesCallsInRule pts r@(PTRule l (PQDes _ _ _pps pq@(PQApp l'' _ pps')) _e) =
    PTRule l pq (PApp l'' (gensym "extract" (namePattern pq) pts) (map toExpr $ concatMap collectVars pps'))    
extractDesCallsInRule _ r = r

extractDesCalls :: [PT] -> PT -> PT
extractDesCalls pts pt@(PTFun l id ts t rs)
    | isMixedRules pt = PTFun l id ts t (map (extractDesCallsInRule pts) rs)
    | otherwise       = pt
extractDesCalls _ pt = pt

desExtract :: [PT] -> [PT]
desExtract pts = (map (extractDesCalls pts) pts) ++ (helperFuns pts)

defunc :: [PT] -> Maybe [PT]
defunc pts
    | any hasIllegalRules pts = Nothing
    | otherwise = EntangledD.defunc (desExtract pts)