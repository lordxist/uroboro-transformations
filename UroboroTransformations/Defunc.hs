module UroboroTransformations.Defunc where

import Data.Maybe

import Uroboro.Tree

import qualified UroboroTransformations.FragmentsForDefunc.MixedDefs.Defunc as MixedDefsD
import qualified UroboroTransformations.FragmentsForDefunc.Entangled.Defunc as EntangledD

import UroboroTransformations.Util
import UroboroTransformations.Util.HelperFuns

import Control.Monad(liftM)
import Control.Monad.Trans.Writer.Lazy

helperFun :: [PT] -> PT -> PTRule -> HelperFuns
helperFun pts (PTFun _ _ ts t _) (PTRule l (PQDes l' id' pps pq@(PQDes l'' des _ _)) e) =
    HelperFuns $ [PTFun l helperFunId (collectVarTypes pts ts pq) (destructorReturnType des pts) [r]]
  where
    helperFunId = gensym "extract" (namePattern pq) pts

    r = PTRule l (PQDes l' id' pps (PQApp l'' helperFunId (collectVarsPQ pq))) e
helperFun _ _ _ = undefined

containsMultiDes :: PT -> Bool
containsMultiDes (PTFun _ _ _ _ rs) = any ruleContainsMultiDes rs
  where
    ruleContainsMultiDes (PTRule _ (PQDes _ _ _ (PQDes _ _ _ _)) _) = True
    ruleContainsMultiDes _ = False
containsMultiDes _ = False

extractOuterDesCallsInRule :: [PT] -> PT -> PTRule -> Writer HelperFuns PTRule
extractOuterDesCallsInRule pts pt r@(PTRule l (PQDes _ _ _ pq@(PQDes l' _ _ _)) _e) =
    writer (replacedRule, helperFun pts pt r)
  where
    replacedRule =
        PTRule l pq (PApp l' (gensym "extract" (namePattern pq) pts) (map toExpr $ collectVarsPQ pq))
extractOuterDesCallsInRule _ _ r = return r

extractOuterDesCalls :: [PT] -> PT -> Writer HelperFuns PT
extractOuterDesCalls pts pt@(PTFun l id ts t rs) = liftM (PTFun l id ts t) (mapM (extractOuterDesCallsInRule pts pt) rs)
extractOuterDesCalls _ pt = return pt

extractOuterDes :: [PT] -> [PT]
extractOuterDes = extractHelperFuns extractOuterDesCalls

elimMultiDes :: [PT] -> [PT]
elimMultiDes pts
    | any containsMultiDes pts = elimMultiDes $ extractOuterDes pts
    | otherwise                = pts

-- |Defunctionalize an Uroboro program
defunc :: [PT] -> Maybe [PT]
defunc pts = EntangledD.defuncLegal $ MixedDefsD.desExtract $ elimMultiDes pts