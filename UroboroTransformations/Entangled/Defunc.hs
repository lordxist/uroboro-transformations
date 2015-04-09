module UroboroTransformations.Entangled.Defunc where

import Uroboro.Tree
import Uroboro.Error

import UroboroTransformations.Util

import qualified UroboroTransformations.CoDataDefsDisj.Defunc as CoDataDefsDisjD

import Data.List(nubBy)

con :: PP -> Bool
con (PPCon _ _ _) = True
con _             = False

isEntangledDesRule :: PTDes -> PTRule -> Bool
isEntangledDesRule des (PTRule _ (PQDes _ id pps (PQApp _ _ pps')) _) =
    (id == (desIdentifier des)) && (any con (pps ++ pps'))
isEntangledDesRule _ _ = False

entangledDesRulesInPT :: PTDes -> PT -> [([PTRule], PT)]
entangledDesRulesInPT des fun@(PTFun _ _ _ _ rs)
    | any (isEntangledDesRule des) rs = [(filter (isEntangledDesRule des) rs, fun)]
    | otherwise = []
entangledDesRulesInPT _ _ = []

entangledDesRules :: [PT] -> PTDes -> ([([PTRule], PT)], PTDes)
entangledDesRules pts des = (concatMap (entangledDesRulesInPT des) pts, des)

helperFunRule :: Identifier -> PTRule -> PTRule
helperFunRule id (PTRule l (PQDes _ _ pps (PQApp l' _ pps')) e) =
    PTRule l (PQApp l' id (pps' ++ pps)) e
helperFunRule _ _ = undefined

helperFuns :: [PT] -> [PT]
helperFuns pts = do
    (desFunRs, des) <- map (entangledDesRules pts) (concatMap destructors pts)
    (desRs, fun) <- desFunRs
    let name = gensym (desIdentifier des) (funIdentifier fun) pts
    let rs = map (helperFunRule name) desRs
    return $ PTFun dummyLocation name ((funArgTypes fun) ++ (desArgTypes des)) (desReturnType des) rs
  where
    desArgTypes (PTDes _ _ _ ts _) = ts

    funArgTypes (PTFun _ _ ts _ _) = ts

    desReturnType (PTDes _ t _ _ _) = t

convertToVars :: [PP] -> Int -> ([PP], Int)
convertToVars [] n = ([], n)
convertToVars (pp:pps) n = do
    let (vars, n') = convertToVars pps (n+1)
    ((PPVar dummyLocation ("x"++(show n))):vars, n')

extractPatternMatchingInRule :: [PT] -> PTRule -> PTRule
extractPatternMatchingInRule pts r@(PTRule l (PQDes l' id pps (PQApp l'' id' pps')) e)
    | any con (pps ++ pps') = do
        let (vars', n) = convertToVars pps' 0
        let (vars, _) = convertToVars pps n
        let helperFunName = gensym id id' pts
        let expr = PApp dummyLocation helperFunName $ map toExpr (vars' ++ vars)
        PTRule l (PQDes l' id vars (PQApp l'' id' vars')) expr
    | otherwise = r
extractPatternMatchingInRule _ r = r

extractPatternMatching :: [PT] -> PT -> PT
extractPatternMatching pts (PTFun l id ts t rs) =
    PTFun l id ts t $ nubBy isSameRuleAs (map (extractPatternMatchingInRule pts) rs)
  where
    (PTRule _ pq e) `isSameRuleAs` (PTRule _ pq2 e2) = (pq `pqEq` pq2)

    (PQApp _ id pps) `pqEq` (PQApp _ id2 pps2) =
        (id == id2) && (pps `ppsEq` pps2)
    (PQDes _ id pps pq) `pqEq` (PQDes _ id2 pps2 pq2) =
        (id == id2) && (pq `pqEq` pq2) && (pps `ppsEq` pps2)        
    _ `pqEq` _ = False

    (PPVar _ id) `ppEq` (PPVar _ id2) = id == id2
    (PPCon _ id pps) `ppEq` (PPCon _ id2 pps2) = (id == id2) && (pps `ppsEq` pps2)
    _ `ppEq` _ = False

    pps `ppsEq` pps2 = ((length pps) == (length pps2)) && (and $ zipWith ppEq pps pps2)
extractPatternMatching _ pt = pt

disentangle :: [PT] -> [PT]
disentangle pts = (map (extractPatternMatching pts) pts) ++ (helperFuns pts)

nested :: PP -> Bool
nested (PPCon _ _ pps) = any con pps
nested _ = False

illegalHolePatternRule :: PTRule -> Bool
illegalHolePatternRule (PTRule _ (PQDes _ _ _ _) _) = True
illegalHolePatternRule (PTRule _ (PQApp _ _ []) _) = True
illegalHolePatternRule (PTRule _ (PQApp _ _ (pp:pps)) _) = (any nested (pp:pps)) || ((not . con) pp)

illegalDesPatternRule :: PTRule -> Bool
illegalDesPatternRule (PTRule _ (PQDes _ _ pps (PQApp _ _ pps')) _) = any nested (pps ++ pps')
illegalDesPatternRule _ = True

hasIllegalRules :: PT -> Bool
hasIllegalRules (PTFun _ _ _ _ rs) = (any illegalDesPatternRule rs) && (any illegalHolePatternRule rs)
hasIllegalRules _ = False

defuncLegal :: [PT] -> Maybe [PT]
defuncLegal pts = CoDataDefsDisjD.defuncLegal (disentangle pts)

-- |Defunctionalize a program in the Entangled Fragment
-- Fails when not in the fragment
defunc :: [PT] -> Maybe [PT]
defunc pts
    | any hasIllegalRules pts = Nothing
    | otherwise = defuncLegal pts