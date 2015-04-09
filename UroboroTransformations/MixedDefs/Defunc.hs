module UroboroTransformations.MixedDefs.Defunc where

import Uroboro.Tree

import UroboroTransformations.Util
import qualified UroboroTransformations.Entangled.Defunc as EntangledD

import Control.Monad(liftM2)

isMixedRules :: PT -> Bool
isMixedRules (PTFun _ _ _ _ rs) =
    (any EntangledD.illegalDesPatternRule rs) && (any EntangledD.illegalHolePatternRule rs)
isMixedRules _ = False

collectVars :: PP -> [PP]
collectVars (PPCon _ _ pps) = pps
collectVars v = [v]

helperFunRule :: Identifier -> PTRule -> [PTRule]
helperFunRule id (PTRule l (PQDes l' id' pps (PQApp l'' _ pps')) e) =
    [PTRule l (PQDes l' id' pps (PQApp l'' id (concatMap collectVars pps'))) e]
helperFunRule _ _ = []

helperFun :: [PT] -> PT -> PT
helperFun pts (PTFun l id ts t rs) = PTFun l helperFunId ts t (concatMap (helperFunRule helperFunId) rs)
  where
    helperFunId = gensym id "extract" pts
helperFun _ _ = undefined

helperFuns :: [PT] -> [PT]
helperFuns pts = map (helperFun pts) (filter isMixedRules pts)

extractDesCallsInRule :: [PT] -> PTRule -> PTRule
extractDesCallsInRule pts r@(PTRule l (PQDes _ id pps pq@(PQApp l'' id' pps')) e) =
    PTRule l pq (PApp l'' (gensym id' "extract" pts) (map toExpr $ concatMap collectVars pps'))    
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