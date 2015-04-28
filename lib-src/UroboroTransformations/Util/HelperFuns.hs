module UroboroTransformations.Util.HelperFuns where

import Uroboro.Tree
import Uroboro.Error

import qualified Data.Map.Lazy as Map
import Data.List(nubBy, groupBy)
import Data.Maybe(fromJust)
import Data.Monoid

import Control.Arrow(second)
import Control.Monad(liftM)
import Control.Monad.Trans.Writer.Lazy

newtype HelperFuns = HelperFuns (Map.Map FunSig [PTRule])

newtype FunSig = FunSig { getFunSig :: (Location, Identifier, [Type], Type) }

makeHelperFuns :: PT -> HelperFuns
makeHelperFuns (PTFun l id ts t rs) = HelperFuns $ Map.singleton (FunSig (l, id, ts, t)) rs

getHelperFuns :: HelperFuns -> [PT]
getHelperFuns (HelperFuns hmap) = map merge $ Map.keys hmap
  where
    merge k@(FunSig (l, id, ts, t)) = PTFun l id ts t (fromJust $ Map.lookup k hmap)

instance Eq FunSig where
    (FunSig (_, id, ts, t)) == (FunSig (_, id', ts', t')) = (id == id') && (ts == ts') && (t == t')

instance Ord FunSig where
    (FunSig (_, id, ts, t)) <= (FunSig (_, id', ts', t')) = id <= id'

instance Monoid HelperFuns where
    (HelperFuns map1) `mappend` (HelperFuns map2) = HelperFuns $ Map.unionWith (++) map1 map2

    mempty = HelperFuns Map.empty

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