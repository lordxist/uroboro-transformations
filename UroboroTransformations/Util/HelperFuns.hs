module UroboroTransformations.Util.HelperFuns where

import Uroboro.Tree

import Data.List(nubBy, groupBy)
import Data.Monoid

import Control.Arrow(second)
import Control.Monad(liftM)
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

extractHelperFunsFromOneFun :: (PT -> PTRule -> Writer HelperFuns PTRule) -> PT -> Writer HelperFuns PT
extractHelperFunsFromOneFun f fun@(PTFun l id ts t rs) = liftM ((PTFun l id ts t) . (nubBy hasSamePatternAs)) $ mapM (f fun) rs
  where
    (PTRule _ pq e) `hasSamePatternAs` (PTRule _ pq2 e2) = (pq `pqEq` pq2)

    (PQApp _ id pps) `pqEq` (PQApp _ id2 pps2) =
        (id == id2) && (pps `ppsEq` pps2)
    (PQDes _ id pps pq) `pqEq` (PQDes _ id2 pps2 pq2) =
        (id == id2) && (pq `pqEq` pq2) && (pps `ppsEq` pps2)        
    _ `pqEq` _ = False

    (PPVar _ _) `ppEq` (PPVar _ _) = True
    (PPCon _ id pps) `ppEq` (PPCon _ id2 pps2) = (id == id2) && (pps `ppsEq` pps2)
    _ `ppEq` _ = False

    pps `ppsEq` pps2 = ((length pps) == (length pps2)) && (and $ zipWith ppEq pps pps2)
extractHelperFunsFromOneFun _ pt = return pt

extractHelperFuns :: ([PT] -> PT -> PTRule -> Writer HelperFuns PTRule) -> [PT] -> [PT]
extractHelperFuns f pts = runExtraction $ mapM (extractHelperFunsFromOneFun $ f pts) pts