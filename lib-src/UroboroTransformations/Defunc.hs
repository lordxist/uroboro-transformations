module UroboroTransformations.Defunc where

import Data.Maybe

import Uroboro.Tree

import UroboroTransformations.Unnest.ForDefunc (unnestForDefunc)
import qualified UroboroTransformations.CoreDR.CoreDefunc as CoreDefunc (defunc)

-- | Defunctionalize an Uroboro program
defunc :: [PT] -> Maybe [PT]
defunc pts = do
    pts' <- unnestForDefunc pts
    CoreDefunc.defunc pts'