module UroboroTransformations where

import Data.Maybe

import Uroboro.Tree

import qualified UroboroTransformations.MixedDefs.Defunc as MixedDefsD
import qualified UroboroTransformations.Entangled.Defunc as EntangledD

-- |Defunctionalize an Uroboro program
-- At the moment, doesn't work with copatterns with multiple destructor calls
defunc :: [PT] -> Maybe [PT]
defunc pts = EntangledD.defuncLegal $ MixedDefsD.desExtract pts