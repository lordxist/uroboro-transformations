module UroboroTransformations.CoDataDefsDisj.FragmentTest where

import Uroboro.Tree

import qualified UroboroTransformations.CoDataFragments.Defunc as CoDataD
import qualified UroboroTransformations.CoDataFragments.Refunc as CoDataR

hasIllegalRules :: PT -> Bool
hasIllegalRules (PTFun _ _ _ _ rs) = (any CoDataD.illegalRule rs) && (any CoDataR.illegalRule rs)
hasIllegalRules _ = False