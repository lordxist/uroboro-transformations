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

isRefunced :: PT -> Bool
isRefunced (PTNeg _ _ _) = True
isRefunced (PTFun _ _ _ _ rs) = all hasDestrPattern rs
isRefunced _ = False

refuncExpInRule :: [(Location, Identifier, [Type], Type)] -> PTRule -> PTRule
refuncExpInRule fsigs (PTRule l pq e) = PTRule l pq (CoDataR.refuncExp e fsigs)

refuncRuleExps :: [(Location, Identifier, [Type], Type)] -> PT -> PT
refuncRuleExps fsigs (PTFun l id ts t rs) = PTFun l id ts t (map (refuncExpInRule fsigs) rs)
refuncRuleExps _ pt = pt

refuncLegal :: [PT] -> Maybe [PT]
refuncLegal pts = do
    let alreadyRefuncedPts = filter isRefunced pts
    let notRefuncedPts = filter (not . isRefunced) pts
    newRefuncedPts <- CoDataR.refunc notRefuncedPts
    return $ (map (refuncRuleExps $ funSigs notRefuncedPts) alreadyRefuncedPts) ++ newRefuncedPts

-- |Refunctionalize a program in the CoData Definitions Disjunction Fragment
-- Fails when not in the fragment
refunc :: [PT] -> Maybe [PT]
refunc pts
    | any hasIllegalRules pts = Nothing
    | otherwise = refuncLegal pts