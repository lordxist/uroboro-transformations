module UroboroTransformations.MixedDefs.Defunc where

import Uroboro.Tree

import UroboroTransformations.Util
import qualified UroboroTransformations.Entangled.Defunc as EntangledD

import Control.Monad(liftM2)
import Data.List(groupBy, intercalate)

isMixedRules :: PT -> Bool
isMixedRules (PTFun _ _ _ _ rs) =
    (any EntangledD.illegalDesPatternRule rs) && (any EntangledD.illegalHolePatternRule rs)
isMixedRules _ = False

collectVars :: PP -> [PP]
collectVars (PPCon _ _ pps) = concatMap collectVars pps
collectVars v = [v]

helperFunRule :: Identifier -> PTRule -> PTRule
helperFunRule id (PTRule l (PQDes l' id' pps (PQApp l'' _ pps')) e) =
    PTRule l (PQDes l' id' pps (PQApp l'' id (concatMap collectVars pps'))) e
helperFunRule _ _ = undefined

-- Problem: not a unique mapping. Can be made to be one, but ideally the names should then
-- be simplified to a nicer form whenever there are no conflicts.
namePattern :: PQ -> String
namePattern (PQApp _ id pps) = id ++ "_" ++ (intercalate "__" (map namePPPattern pps))

namePPPattern :: PP -> String
namePPPattern (PPVar _ _) = ""
namePPPattern (PPCon _ id pps) = id ++ "_" ++ (intercalate "__" (map namePPPattern pps))

-- Requires globally unique constructor names. Otherwise collectVarTypes needs to be implemented
-- using typed syntax trees (and the transformation is run on the typecheck result).
constructorTypes :: Identifier -> [PT] -> [Type]
constructorTypes id pts = types ((concatMap getConstructorForId pts) !! 0)
  where
    getConstructorForId (PTPos l t ((con@(PTCon _ _ id' _)):cons))
        | id' == id = [con]
        | otherwise = getConstructorForId (PTPos l t cons)
    getConstructorForId _ = []

    types (PTCon _ _ _ ts) = ts

collectVarTypes :: [PT] -> [Type] -> PQ -> [Type]
collectVarTypes pts ts (PQApp _ _ pps) = concatMap varTypes (zip pps ts)
  where
    varTypes ((PPVar _ _), t)       = [t]
    varTypes ((PPCon _ id pps'), _) = concatMap varTypes (zip pps' (constructorTypes id pts))
collectVarTypes _ _ _ = undefined

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

hasIllegalRules :: PT -> Bool
hasIllegalRules (PTFun _ _ _ _ rs) =
    any (liftM2 (&&) EntangledD.illegalHolePatternRule EntangledD.illegalDesPatternRule) rs
hasIllegalRules _ = False

defunc :: [PT] -> Maybe [PT]
defunc pts
    | any hasIllegalRules pts = Nothing
    | otherwise = EntangledD.defunc (desExtract pts)