module UroboroTransformations.Refunc where

import Uroboro.Tree

-- old version
{-
import UroboroTransformations.Util.HelperFuns
import UroboroTransformations.Util.DesExtraction

import qualified UroboroTransformations.FragmentsForRefunc.Entangled.Refunc as EntangledR

import Data.List(partition)
import Control.Arrow(first)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Lazy

isMixedRules :: PT -> Bool
isMixedRules (PTFun _ _ _ _ rs) = (any hasDesPattern rs) && (any hasConPattern rs)
  where
    hasDesPattern (PTRule _ (PQDes _ _ _ _) _) = True
    hasDesPattern _                            = False

    hasConPattern (PTRule _ (PQApp _ _ _) _) = True
    hasConPattern _                          = False
isMixedRules _                  = True

extractAllDesCalls :: PT -> PTRule -> ReaderT [PT] (Writer HelperFuns) PTRule
extractAllDesCalls fun r@(PTRule _ (PQDes _ _ _ _) _) = do
    replacedRule <- extractDesCalls fun r
    extractAllDesCalls fun replacedRule
extractAllDesCalls _ r = return r

elimDesFromMixeds :: [PT] -> [PT]
elimDesFromMixeds pts = uncurry (++) $ first (extractHelperFuns extractAllDesCalls) $ partition isMixedRules pts

-- |Refunctionalize an Uroboro program
refunc :: [PT] -> Maybe [PT]
refunc pts = EntangledR.refuncLegal $ elimDesFromMixeds pts
-}

import UroboroTransformations.Unnest.ForRefunc (unnestForRefunc)
import UroboroTransformations.MoveCon (moveConFront)
import UroboroTransformations.CoDataDefsDisj.Refunc (refuncLegal)

-- | Refunctionalize an Uroboro program
refunc :: [PT] -> Maybe [PT]
refunc pts = do
    pts' <- unnestForRefunc pts
    pts'' <- moveConFront pts'
    refuncLegal pts''
