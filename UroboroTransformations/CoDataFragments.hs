module UroboroTransformations.CoDataFragments where

import Uroboro.Tree
import Uroboro.Error

isFun :: PT -> Bool
isFun (PTFun _ _ _ _ _) = True
isFun _ = False

funSigs :: [PT] -> [(Location, Identifier, [Type], Type)]
funSigs = (map sig) . (filter isFun)
  where
    sig (PTFun l id ts t _) = (l, id, ts, t)
    sig _ = undefined

-- |Retrieves all rules from the parse trees.
-- |Fails when a rule is illegal according to the supplied predicate.
funRules :: [PT] -> (PTRule -> Bool) -> Maybe [[PTRule]]
funRules pts ill = (mapM rules (filter isFun pts))
  where
    rules (PTFun _ _ _ _ rs)
        | any ill rs = Nothing
        | otherwise          = Just rs
    rules _ = undefined