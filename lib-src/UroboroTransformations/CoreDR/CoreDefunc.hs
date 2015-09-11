module UroboroTransformations.CoreDR.CoreDefunc where

import Uroboro.Tree

import qualified UroboroTransformations.CoDataFragments.Defunc as CoDataD

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

-- | Defunctionalize a sufficiently unnested program
-- Fails when not in the sufficiently unnested for defunc. fragment
defunc :: [PT] -> Maybe [PT]
defunc pts = do
    let alreadyDefuncedPts = filter isDefunced pts
    let notDefuncedPts = filter (not . isDefunced) pts
    newDefuncedPts <- CoDataD.defunc notDefuncedPts
    return $ (map defuncRuleExps alreadyDefuncedPts) ++ newDefuncedPts