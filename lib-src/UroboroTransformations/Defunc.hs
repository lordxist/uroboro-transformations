module UroboroTransformations.Defunc where

import Data.Maybe

import Uroboro.Tree

import UroboroTransformations.Unnest.ForDefunc (unnestForDefunc)
import qualified UroboroTransformations.CoreDR.CoreDefunc as CoreDefunc (defunc)

-- | Defunctionalize an Uroboro program. Note: To defunc. terms, use
-- 'UroboroTransformations.CoDataFragments.Defunc.defuncExp'.
defunc :: [PT] -> Maybe [PT]
defunc pts = do
    pts' <- unnestForDefunc pts
    CoreDefunc.defunc pts'