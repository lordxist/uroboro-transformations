module UroboroTransformations.CoDataDefsDisj.Defunc where

import Uroboro.Tree

import UroboroTransformations.CoDataDefsDisj.FragmentTest

import qualified UroboroTransformations.CoDataFragments.Defunc as CoDataD
import qualified UroboroTransformations.CoDataFragments.Refunc as CoDataR

hasHolePattern :: PTRule -> Bool
hasHolePattern (PTRule _ (PQApp _ _ _) _) = True
hasHolePattern _ = False

isDefunced :: PT -> Bool
isDefunced (PTPos _ _ _) = True
isDefunced (PTFun _ _ _ _ rs) = all hasHolePattern rs
isDefunced _ = False

defuncExpInRule :: PTRule -> PTRule
defuncExpInRule (PTRule l pq e) = PTRule l pq (CoDataD.defuncExp e)

defuncRuleExps :: PT -> PT
defuncRuleExps (PTFun l id ts t rs) = PTFun l id ts t (map defuncExpInRule rs)
defuncRuleExps pt = pt

defuncLegal :: [PT] -> Maybe [PT]
defuncLegal pts = do
    let alreadyDefuncedPts = filter isDefunced pts
    let notDefuncedPts = filter (not . isDefunced) pts
    newDefuncedPts <- CoDataD.defunc notDefuncedPts
    return $ (map defuncRuleExps alreadyDefuncedPts) ++ newDefuncedPts

-- |Defunctionalize a program in the CoData Definitions Disjunction Fragment
-- Fails when not in the fragment
defunc :: [PT] -> Maybe [PT]
defunc pts
    | any hasIllegalRules pts = Nothing
    | otherwise = defuncLegal pts