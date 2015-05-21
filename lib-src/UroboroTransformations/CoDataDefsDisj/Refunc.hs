module UroboroTransformations.CoDataDefsDisj.Refunc where

import Uroboro.Tree
import Uroboro.Error

import UroboroTransformations.CoDataDefsDisj.FragmentTest

import qualified UroboroTransformations.CoDataFragments.Defunc as CoDataD
import qualified UroboroTransformations.CoDataFragments.Refunc as CoDataR

import UroboroTransformations.CoDataFragments

import Debug.Trace(trace)

hasDestrPattern :: PTRule -> Bool
hasDestrPattern (PTRule _ (PQDes _ _ _ _) _) = True
hasDestrPattern _ = False

isCodataType :: Type -> [PT] -> Bool
isCodataType t ((PTNeg _ t' _):pts)
        | t' == t   = True
        | otherwise = isCodataType t pts
isCodataType _ _ = False

isRefunced :: [PT] -> PT -> Bool
isRefunced _ (PTNeg _ _ _) = True
isRefunced pts (PTFun _ _ (t:_) _ rs) = (isCodataType t pts) || (all hasDestrPattern rs)
isRefunced _ (PTFun _ _ [] _ _) = True
isRefunced _ _ = False

refuncExpInRule :: [(Location, Identifier, [Type], Type)] -> PTRule -> PTRule
refuncExpInRule fsigs (PTRule l pq e) = PTRule l pq (CoDataR.refuncExp e fsigs)

refuncRuleExps :: [(Location, Identifier, [Type], Type)] -> PT -> PT
refuncRuleExps fsigs (PTFun l id ts t rs) = PTFun l id ts t (map (refuncExpInRule fsigs) rs)
refuncRuleExps _ pt = pt

refuncLegal :: [PT] -> Maybe [PT]
refuncLegal pts = do
    let alreadyRefuncedPts = filter (isRefunced pts) pts
    let notRefuncedPts = filter (not . (isRefunced pts)) pts
    newRefuncedPts <- CoDataR.refunc notRefuncedPts
    return $ (map (refuncRuleExps $ funSigs notRefuncedPts) alreadyRefuncedPts) ++ newRefuncedPts

-- |Refunctionalize a program in the CoData Definitions Disjunction Fragment
-- Fails when not in the fragment
refunc :: [PT] -> Maybe [PT]
refunc pts
    | any hasIllegalRules pts = Nothing
    | otherwise = refuncLegal pts