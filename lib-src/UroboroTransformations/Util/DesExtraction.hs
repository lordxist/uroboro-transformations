module UroboroTransformations.Util.DesExtraction(extractDesCalls) where

import Uroboro.Tree

import UroboroTransformations.Util
import UroboroTransformations.Util.HelperFuns

import Control.Monad(liftM)
import Control.Monad.Trans.Writer.Lazy

import Debug.Trace(trace)

helperFunRule :: Identifier -> PTRule -> PTRule
helperFunRule id (PTRule l (PQDes l' id' pps pq) e) =
    PTRule l (PQDes l' id' pps (PQApp l' id (collectVarsPQ pq))) e
helperFunRule _ _ = undefined

helperFun :: [PT] -> PT -> PTRule -> HelperFuns
helperFun pts (PTFun _ _ ts t _) r@(PTRule l (PQDes _ _ _ pq) _) =
    makeHelperFuns $ PTFun l helperFunId (collectVarTypes pts ts pq) t [(helperFunRule helperFunId r)]
  where
    helperFunId = gensym "extract" (namePattern pq) pts
helperFun _ _ _ = undefined

extractDesCalls :: [PT] -> PT -> PTRule -> Writer HelperFuns PTRule
extractDesCalls pts fun r@(PTRule l (PQDes l' _ _ pq) _e) =
    writer (replacedRule, (helperFun pts fun r))
  where
    replacedRule =
        PTRule l pq (PApp l' (gensym "extract" (namePattern pq) pts) (map toExpr $ collectVarsPQ pq))
extractDesCalls _ _ r = return r
