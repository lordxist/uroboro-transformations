module UroboroTransformations.Refunc where

import Uroboro.Tree

import UroboroTransformations.Util.HelperFuns
import UroboroTransformations.Util.DesExtraction

import qualified UroboroTransformations.FragmentsForRefunc.Entangled.Refunc as EntangledR

import Data.List(partition)
import Control.Arrow(first)
import Control.Monad.Trans.Writer.Lazy

isMixedRules :: PT -> Bool
isMixedRules pt = False -- TODO: implement

extractAllDesCalls :: [PT] -> PT -> PTRule -> Writer HelperFuns PTRule
extractAllDesCalls pts fun r@(PTRule l (PQDes _ _ _ _) _) = do
    replacedRule <- extractDesCalls pts fun r
    extractAllDesCalls pts fun replacedRule

elimDesFromMixeds :: [PT] -> [PT]
elimDesFromMixeds pts = uncurry (++) $ first (extractHelperFuns extractAllDesCalls) $ partition isMixedRules pts

-- |Refunctionalize an Uroboro program
refunc :: [PT] -> Maybe [PT]
refunc pts = EntangledR.refuncLegal $ elimDesFromMixeds pts