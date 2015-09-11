module UroboroTransformations.CoreDR.FragmentTests where

import Control.Arrow

import Uroboro.Tree

import UroboroTransformations.Unnest.ForDefunc
import UroboroTransformations.Unnest.ForRefunc

-- |Tests whether the program is sufficiently unnested for core defunc.
isUnnestedD :: [PT] -> Bool
isUnnestedD = all (not.isDNestedFunDef)

-- |Tests whether the program is sufficiently unnested for core refunc.
isUnnestedR :: [PT] -> Bool
isUnnestedR = all (not.isRNestedFunDef)
