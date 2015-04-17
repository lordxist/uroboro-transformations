module UroboroTransformations.Entangled.FragmentTest where

import Uroboro.Tree

import UroboroTransformations.Util

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