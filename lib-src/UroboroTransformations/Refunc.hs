module UroboroTransformations.Refunc where

import Uroboro.Tree

import UroboroTransformations.Unnest.ForRefunc (unnestForRefunc)
import UroboroTransformations.CoreDR.MoveCon (moveConFront)
import qualified UroboroTransformations.CoreDR.CoreRefunc as CoreRefunc (refunc)

-- | Refunctionalize an Uroboro program. Note: To refunc. terms, use
-- 'UroboroTransformations.CoDataFragments.Refunc.refuncExp'.
refunc :: [PT] -> Maybe [PT]
refunc pts = do
    pts' <- unnestForRefunc pts
    pts'' <- moveConFront pts'
    CoreRefunc.refunc pts''
