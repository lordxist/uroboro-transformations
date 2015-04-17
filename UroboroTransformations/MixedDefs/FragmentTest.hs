module UroboroTransformations.MixedDefs.FragmentTest where

import Uroboro.Tree

import qualified UroboroTransformations.Entangled.FragmentTest as EntangledTest

import Control.Monad(liftM2)

hasIllegalRules :: PT -> Bool
hasIllegalRules (PTFun _ _ _ _ rs) =
    any (liftM2 (&&) EntangledTest.illegalHolePatternRule EntangledTest.illegalDesPatternRule) rs
hasIllegalRules _ = False