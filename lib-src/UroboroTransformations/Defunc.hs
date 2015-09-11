module UroboroTransformations.Defunc where

import Data.Maybe

import Uroboro.Tree

-- old version
{-
import qualified UroboroTransformations.FragmentsForDefunc.MixedDefs.Defunc as MixedDefsD
import qualified UroboroTransformations.FragmentsForDefunc.Entangled.Defunc as EntangledD

import UroboroTransformations.Util
import UroboroTransformations.Util.HelperFuns
import UroboroTransformations.Util.DesExtraction

import Control.Monad(liftM)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Lazy

extractOuterDesCalls :: PT -> PTRule -> ReaderT [PT] (Writer HelperFuns) PTRule
extractOuterDesCalls pt r@(PTRule l (PQDes _ _ _ pq@(PQDes l' _ _ _)) _e) = do
    replacedRule <- extractDesCalls pt r
    extractOuterDesCalls pt replacedRule
extractOuterDesCalls _ r = return r

elimMultiDes :: [PT] -> [PT]
elimMultiDes = extractHelperFuns extractOuterDesCalls

-- |Defunctionalize an Uroboro program
defunc :: [PT] -> Maybe [PT]
defunc pts = EntangledD.defuncLegal $ MixedDefsD.desExtract $ elimMultiDes pts
-}

import UroboroTransformations.Unnest.ForDefunc (unnestForDefunc)
import UroboroTransformations.CoDataDefsDisj.Defunc (defuncLegal)

-- | Defunctionalize an Uroboro program
defunc :: [PT] -> Maybe [PT]
defunc pts = do
    pts' <- unnestForDefunc pts
    defuncLegal pts'