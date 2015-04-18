module UroboroTransformations.Util.HelperFuns where

import Uroboro.Tree

import Data.List(groupBy)
import Data.Monoid

import Control.Arrow(second)
import Control.Monad.Trans.Writer.Lazy

newtype HelperFuns = HelperFuns { getHelperFuns :: [PT] }

instance Monoid HelperFuns where
    (HelperFuns pts) `mappend` (HelperFuns pts2) = HelperFuns $ map merge $ groupBy sameFun (pts ++ pts2)
      where
        (PTFun _ id ts t _) `sameFun` (PTFun _ id' ts' t' _) = (id == id') && (ts == ts') && (t == t')

        merge funs@((PTFun l id ts t _):_) = PTFun l id ts t (concatMap rules funs)

        rules (PTFun _ _ _ _ rs) = rs

    mempty = HelperFuns []

runExtraction :: Writer HelperFuns [PT] -> [PT]
runExtraction = (uncurry (++)) . (second getHelperFuns) . runWriter

extractHelperFuns :: ([PT] -> PT -> Writer HelperFuns PT) -> [PT] -> [PT]
extractHelperFuns f pts = runExtraction $ mapM (f pts) pts