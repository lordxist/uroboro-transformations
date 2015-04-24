module UroboroTransformations.FragmentsForDefunc.MixedDefs.Defunc where

import Uroboro.Tree

import UroboroTransformations.Util
import UroboroTransformations.Util.HelperFuns
import UroboroTransformations.Util.DesExtraction
import qualified UroboroTransformations.FragmentsForDefunc.Entangled.Defunc as EntangledD
import qualified UroboroTransformations.FragmentsForDefunc.Entangled.FragmentTest as EntangledTest

import UroboroTransformations.FragmentsForDefunc.MixedDefs.FragmentTest

import Data.List(partition)
import Control.Arrow(first)
import Control.Monad(liftM)
import Control.Monad.Trans.Writer.Lazy

isMixedRules :: PT -> Bool
isMixedRules (PTFun _ _ _ _ rs) =
    (any EntangledTest.illegalDesPatternRule rs) && (any EntangledTest.illegalHolePatternRule rs)
isMixedRules _ = True

desExtract :: [PT] -> [PT]
desExtract pts = uncurry (++) $ first (extractHelperFuns extractDesCalls) $ partition isMixedRules pts

defunc :: [PT] -> Maybe [PT]
defunc pts
    | any hasIllegalRules pts = Nothing
    | otherwise = EntangledD.defunc (desExtract pts)