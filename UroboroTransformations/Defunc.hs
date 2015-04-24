module UroboroTransformations.Defunc where

import Data.Maybe

import Uroboro.Tree

import qualified UroboroTransformations.FragmentsForDefunc.MixedDefs.Defunc as MixedDefsD
import qualified UroboroTransformations.FragmentsForDefunc.Entangled.Defunc as EntangledD

import UroboroTransformations.Util
import UroboroTransformations.Util.HelperFuns
import UroboroTransformations.Util.DesExtraction

import Control.Monad(liftM)
import Control.Monad.Trans.Writer.Lazy

extractOuterDesCalls :: [PT] -> PT -> PTRule -> Writer HelperFuns PTRule
extractOuterDesCalls pts pt r@(PTRule l (PQDes _ _ _ pq@(PQDes l' _ _ _)) _e) = do
    replacedRule <- extractDesCalls pts pt r
    extractOuterDesCalls pts pt replacedRule
extractOuterDesCalls _ _ r = return r

elimMultiDes :: [PT] -> [PT]
elimMultiDes = extractHelperFuns extractOuterDesCalls

-- |Defunctionalize an Uroboro program
defunc :: [PT] -> Maybe [PT]
defunc pts = EntangledD.defuncLegal $ MixedDefsD.desExtract $ elimMultiDes pts