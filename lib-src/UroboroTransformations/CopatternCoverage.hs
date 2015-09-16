module UroboroTransformations.CopatternCoverage (
      checkCoverage
    , zipCoverageRules
) where

import Control.Monad.Reader
import Data.Maybe
import Data.List (find)
import Data.Set (fromList)

import Uroboro.Tree
import Uroboro.Checker

import UroboroTransformations.CopatternCoverage.CCTree
import UroboroTransformations.Util
import UroboroTransformations.Util.Conversion

leavesEqualPQs :: [PQ] -> CCTree -> Bool
leavesEqualPQs pqs tree = (fromList (map tqToPQ (leaves tree))) == (fromList pqs)

checkCoverage :: PTSig -> PT -> Reader Program (Maybe CCTree)
checkCoverage sig (PTFun _ _ _ _ rs) = (liftM $ find (leavesEqualPQs (map lhs rs))) searchSpace
  where
    searchSpace = possibleTrees sig (maximum $ map (splittingDepth . lhs) rs)

    lhs (PTRule _ pq _) = pq

fitVariablesTP :: TP -> PP -> TP
fitVariablesTP (TPVar t _) (PPVar _ id) = TPVar t id
fitVariablesTP (TPCon t id tps) (PPCon _ _ pps) = TPCon t id (map (uncurry fitVariablesTP) (zip tps pps))

fitVariablesTQ :: TQ -> PQ -> TQ
fitVariablesTQ (TQDes t id tps tq) (PQDes _ _ pps pq) =
  TQDes t id (map (uncurry fitVariablesTP) (zip tps pps)) (fitVariablesTQ tq pq)
fitVariablesTQ (TQApp t id tps) (PQApp _ _ pps) =
  TQApp t id (map (uncurry fitVariablesTP) (zip tps pps))

zipCoverageRules :: [TQ] -> [PTRule] -> [(PTRule, TQ)]
zipCoverageRules [] _ = []
zipCoverageRules (tq:tqs) rs
    = (fitVariables ((fromJust $ find fitsWithTQ rs),tq)):(zipCoverageRules tqs rs)
  where
    fitsWithTQ (PTRule _ pq _) = (tqToPQ tq) == pq

    fitVariables (r@(PTRule _ pq _), tq) = (r, (fitVariablesTQ tq pq))
