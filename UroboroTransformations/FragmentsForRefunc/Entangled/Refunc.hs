module UroboroTransformations.FragmentsForRefunc.Entangled.Refunc where

import Uroboro.Tree

import qualified UroboroTransformations.CoDataDefsDisj.Refunc as CoDataDefsDisjR

import UroboroTransformations.Util
import UroboroTransformations.Util.HelperFuns

import Data.List(nubBy, groupBy)
import Control.Monad(liftM)
import Control.Monad.Trans.Writer.Lazy

import Debug.Trace(trace)

extractWith :: (PQ -> PQ) -> (PQ -> [PP]) ->[PT] -> PT -> PTRule -> Writer HelperFuns PTRule
extractWith removeLeftCon varsAndLeftCon pts (PTFun l id ts t _) r@(PTRule l' pq e) = do
    let helperRule = PTRule l (PQApp l' helperFunName (varsAndLeftCon pq)) e
    let rt = case pq of (PQDes _ des _ _) -> destructorReturnType des pts
                        (PQApp _ _ _)     -> t
    let helperFuns = HelperFuns [PTFun l helperFunName (collectVarTypes pts ts (removeLeftCon pq)) rt [helperRule]]
    tell helperFuns
    return $ PTRule l (removeLeftCon pq) (PApp dummyLocation helperFunName $ map toExpr $ collectVarsPQ (removeLeftCon pq))
  where
    helperFunName = gensym "extract" (namePattern (removeLeftCon pq)) pts

extractPatternMatchingInRule :: [PT] -> PT -> PTRule -> Writer HelperFuns PTRule
extractPatternMatchingInRule pts fun r@(PTRule _ (PQDes _ _ pps pq) _)
    | any con (pps ++ (ppsForPQ pq)) = extractWith removeLeftCon varsAndLeftCon pts fun r
    | otherwise = return r
  where
    removeLeftCon (PQApp l id pps) = PQApp l id (removeLeftConPPs pps)
    removeLeftCon (PQDes l id pps pq)
        | conInPQ pq = PQDes l id pps (removeLeftCon pq)
        | otherwise  = PQDes l id (removeLeftConPPs pps) pq

    varsAndLeftCon (PQApp _ _ pps) = varsAndLeftConPPs pps
    varsAndLeftCon (PQDes _ _ pps pq)
        | conInPQ pq = (varsAndLeftCon pq) ++ (concatMap collectVars pps)
        | otherwise  = (collectVarsPQ pq) ++ (varsAndLeftConPPs pps)

    varsAndLeftConPPs ((v@(PPVar _ _)):pps) = v:(varsAndLeftConPPs pps)
    varsAndLeftConPPs ((c@(PPCon l id pps)):pps')
        | any con pps = (varsAndLeftConPPs pps) ++ (concatMap collectVars pps')
        | otherwise   = c:(concatMap collectVars pps')
    varsAndLeftConPPs [] = []

    conInPQ (PQDes _ _ pps pq) = (any con pps) || (conInPQ pq)
    conInPQ (PQApp _ _ pps)    = any con pps

    removeLeftConPPs ((v@(PPVar _ _)):pps) = v:(removeLeftConPPs pps)
    removeLeftConPPs ((c@(PPCon l id pps)):pps')
        | any con pps = (PPCon l id (removeLeftConPPs pps)):pps'
        | otherwise   = (PPVar l id):pps' -- TODO: after this, all variable names need to be renamed
    removeLeftConPPs [] = []

    ppsForPQ (PQDes _ _ pps pq) = pps ++ (ppsForPQ pq)
    ppsForPQ (PQApp _ _ pps)    = pps

extractPatternMatchingInRule pts fun r@(PTRule _ (PQApp _ _ pps) _)
    | hasTwoCons pps = extractWith removeLeftCon varsAndLeftCon pts fun r
    | otherwise = return r
  where
    removeLeftCon = undefined

    varsAndLeftCon = undefined

    hasTwoCons = undefined

extractPatternMatchingInRules :: [PT] -> PT -> [PTRule] -> Writer HelperFuns [PTRule]
extractPatternMatchingInRules pts fun rs =
    liftM (nubBy hasSamePatternAs) (mapM (extractPatternMatchingInRule pts fun) rs)
  where
    (PTRule _ pq e) `hasSamePatternAs` (PTRule _ pq2 e2) = (pq `pqEq` pq2)

    (PQApp _ id pps) `pqEq` (PQApp _ id2 pps2) =
        (id == id2) && (pps `ppsEq` pps2)
    (PQDes _ id pps pq) `pqEq` (PQDes _ id2 pps2 pq2) =
        (id == id2) && (pq `pqEq` pq2) && (pps `ppsEq` pps2)        
    _ `pqEq` _ = False

    (PPVar _ _) `ppEq` (PPVar _ _) = True
    (PPCon _ id pps) `ppEq` (PPCon _ id2 pps2) = (id == id2) && (pps `ppsEq` pps2)
    _ `ppEq` _ = False

    pps `ppsEq` pps2 = ((length pps) == (length pps2)) && (and $ zipWith ppEq pps pps2)

extractPatternMatching :: [PT] -> PT -> Writer HelperFuns PT
extractPatternMatching pts fun@(PTFun l id ts t rs) =
    liftM (PTFun l id ts t) (extractPatternMatchingInRules pts fun rs)
extractPatternMatching _ pt = return pt

disentangle :: [PT] -> [PT]
disentangle = extractHelperFuns extractPatternMatching

refuncLegal :: [PT] -> Maybe [PT]
refuncLegal pts = CoDataDefsDisjR.refunc (disentangle pts)