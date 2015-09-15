module UroboroTransformations.CoreDR.CoreRefunc where

import Uroboro.Tree
import Uroboro.Error

import qualified UroboroTransformations.CoDataFragments.Refunc as CoDataR

import UroboroTransformations.Util

hasDestrPattern :: PTRule -> Bool
hasDestrPattern (PTRule _ (PQDes _ _ _ _) _) = True
hasDestrPattern _ = False

isRefunced :: [PT] -> PT -> Bool
isRefunced _ (PTNeg _ _ _) = True
isRefunced pts (PTFun _ _ (t:_) _ (r:[])) = True
isRefunced pts (PTFun _ _ (t:_) _ rs) = all hasDestrPattern rs
isRefunced _ (PTFun _ _ [] _ _) = True
isRefunced _ _ = False

refuncExpInRule :: [(Location, Identifier, [Type], Type)] -> PTRule -> PTRule
refuncExpInRule fsigs (PTRule l pq e) = PTRule l pq (CoDataR.refuncExp e fsigs)

refuncRuleExps :: [(Location, Identifier, [Type], Type)] -> PT -> PT
refuncRuleExps fsigs (PTFun l id ts t rs) = PTFun l id ts t (map (refuncExpInRule fsigs) rs)
refuncRuleExps _ pt = pt

-- | Refunctionalize a sufficiently unnested program
-- Fails when not in the sufficiently unnested for refunc. fragment
refunc :: [PT] -> Maybe [PT]
refunc pts = do
    let alreadyRefuncedPts = filter (isRefunced pts) pts
    let notRefuncedPts = filter (not . (isRefunced pts)) pts
    newRefuncedPts <- CoDataR.refunc notRefuncedPts
    return $ (map (refuncRuleExps $ funSigs notRefuncedPts) alreadyRefuncedPts) ++ newRefuncedPts