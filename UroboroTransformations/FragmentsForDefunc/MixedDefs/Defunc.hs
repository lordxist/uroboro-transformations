module UroboroTransformations.FragmentsForDefunc.MixedDefs.Defunc where

import Uroboro.Tree

import UroboroTransformations.Util
import UroboroTransformations.Util.HelperFuns
import qualified UroboroTransformations.FragmentsForDefunc.Entangled.Defunc as EntangledD
import qualified UroboroTransformations.FragmentsForDefunc.Entangled.FragmentTest as EntangledTest

import UroboroTransformations.FragmentsForDefunc.MixedDefs.FragmentTest

import Data.List(groupBy, intercalate)
import Control.Monad(liftM)
import Control.Monad.Trans.Writer.Lazy

isMixedRules :: PT -> Bool
isMixedRules (PTFun _ _ _ _ rs) =
    (any EntangledTest.illegalDesPatternRule rs) && (any EntangledTest.illegalHolePatternRule rs)
isMixedRules _ = False

helperFunRule :: Identifier -> PTRule -> PTRule
helperFunRule id (PTRule l (PQDes l' id' pps (PQApp l'' _ pps')) e) =
    PTRule l (PQDes l' id' pps (PQApp l'' id (concatMap collectVars pps'))) e
helperFunRule _ _ = undefined

helperFun :: [PT] -> PT -> PTRule -> HelperFuns
helperFun pts (PTFun _ _ ts t _) r@(PTRule l (PQDes _ _ _ pq) _) =
    HelperFuns [PTFun l helperFunId (collectVarTypes pts ts pq) t [(helperFunRule helperFunId r)]]
  where
    helperFunId = gensym "extract" (namePattern pq) pts
helperFun _ _ _ = undefined

extractDesCallsInRule :: [PT] -> PT -> PTRule -> Writer HelperFuns PTRule
extractDesCallsInRule pts fun r@(PTRule l (PQDes _ _ _pps pq@(PQApp l'' _ pps')) _e) =
    writer (replacedRule, (helperFun pts fun r))
  where
    replacedRule =
        PTRule l pq (PApp l'' (gensym "extract" (namePattern pq) pts) (map toExpr $ concatMap collectVars pps'))
extractDesCallsInRule _ _ r = return r

extractDesCalls :: [PT] -> PT -> Writer HelperFuns PT
extractDesCalls pts pt@(PTFun l id ts t rs)
    | isMixedRules pt = liftM (PTFun l id ts t) (mapM (extractDesCallsInRule pts pt) rs)
    | otherwise       = return pt
extractDesCalls _ pt  = return pt

desExtract :: [PT] -> [PT]
desExtract = extractHelperFuns extractDesCalls

defunc :: [PT] -> Maybe [PT]
defunc pts
    | any hasIllegalRules pts = Nothing
    | otherwise = EntangledD.defunc (desExtract pts)