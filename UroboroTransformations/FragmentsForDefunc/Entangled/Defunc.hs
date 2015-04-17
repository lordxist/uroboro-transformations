module UroboroTransformations.FragmentsForDefunc.Entangled.Defunc where

import Uroboro.Tree
import Uroboro.Error

import UroboroTransformations.Util
import UroboroTransformations.FragmentsForDefunc.Entangled.FragmentTest

import qualified UroboroTransformations.CoDataDefsDisj.Defunc as CoDataDefsDisjD

import Data.List(nubBy, groupBy)
import Data.Monoid
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Trans.Writer.Lazy

newtype HelperFuns = HelperFuns { getHelperFuns :: [PT] }

instance Monoid HelperFuns where
    (HelperFuns pts) `mappend` (HelperFuns pts2) = HelperFuns $ map merge $ groupBy sameFun (pts ++ pts2)
      where
        (PTFun _ id ts t _) `sameFun` (PTFun _ id' ts' t' _) = (id == id') && (ts == ts') && (t == t')

        merge funs@((PTFun l id ts t _):_) = PTFun l id ts t (concatMap rules funs)

        rules (PTFun _ _ _ _ rs) = rs

    mempty = HelperFuns []

ruleToHelperFuns :: PT -> [Type] -> Type -> Maybe PTRule -> HelperFuns
ruleToHelperFuns fun desTs desRT (Just r@(PTRule l (PQApp _ id _) _)) =
    HelperFuns [(PTFun l id ((funArgTypes fun) ++ desTs) desRT [r])]
  where
    funArgTypes (PTFun _ _ ts _ _) = ts
ruleToHelperFuns _ _ _ Nothing = mempty
ruleToHelperFuns _ _ _ _ = undefined

helperFunRule :: Identifier -> PTRule -> Maybe PTRule
helperFunRule id (PTRule l (PQDes _ _ pps (PQApp l' _ pps')) e) =
    Just $ PTRule l (PQApp l' id (pps' ++ pps)) e
helperFunRule _ _ = Nothing

convertToVar :: PP -> State Int PP
convertToVar _ = do
    n <- get
    modify (+1)
    return $ PPVar dummyLocation ("x"++(show n))

extractPatternMatchingInRule :: [PT] -> PT -> PTRule -> Writer HelperFuns PTRule
extractPatternMatchingInRule pts fun r@(PTRule l (PQDes l' id pps (PQApp l'' id' pps')) e)
    | any con (pps ++ pps') = do
        let (vars', n) = runState (mapM convertToVar pps') 0
        let vars = evalState (mapM convertToVar pps) n
        let helperFunName = gensym id id' pts
        let expr = PApp dummyLocation helperFunName $ map toExpr (vars' ++ vars)
        let newRule = PTRule l (PQDes l' id vars (PQApp l'' id' vars')) expr
        let desTs = destructorTypes id pts
        let desRT = destructorReturnType id pts
        writer (newRule, (ruleToHelperFuns fun desTs desRT $ helperFunRule helperFunName r))
    | otherwise = return r
extractPatternMatchingInRule _ _ r = return r

extractPatternMatchingInRules :: [PT] -> PT -> [PTRule] -> Writer HelperFuns [PTRule]
extractPatternMatchingInRules pts fun rs =
    liftM (nubBy hasSamePatternAs) (mapM (extractPatternMatchingInRule pts fun) rs)
  where
    (PTRule _ pq e) `hasSamePatternAs` (PTRule _ pq2 e2) = (pq `pqEq` pq2)

    (PQApp _ id pps) `pqEq` (PQApp _ id2 pps2) =
        (id == id2) && (pps `ppsEq` pps2)
    (PQDes _ id pps pq) `pqEq` (PQDes _ id2 pps2 pq2) =
        (id == id2) && (pq `pqEq` pq2) && (pps `ppsEq` pps2)        
    _ `pqEq` _ = False

    (PPVar _ id) `ppEq` (PPVar _ id2) = id == id2
    (PPCon _ id pps) `ppEq` (PPCon _ id2 pps2) = (id == id2) && (pps `ppsEq` pps2)
    _ `ppEq` _ = False

    pps `ppsEq` pps2 = ((length pps) == (length pps2)) && (and $ zipWith ppEq pps pps2)

extractPatternMatching :: [PT] -> PT -> Writer HelperFuns PT
extractPatternMatching pts fun@(PTFun l id ts t rs) =
    liftM (PTFun l id ts t) (extractPatternMatchingInRules pts fun rs)
extractPatternMatching _ pt = return pt

disentangle :: [PT] -> [PT]
disentangle pts = (\(x, y) -> x ++ (getHelperFuns y)) $ runWriter (mapM (extractPatternMatching pts) pts)

defuncLegal :: [PT] -> Maybe [PT]
defuncLegal pts = CoDataDefsDisjD.defuncLegal (disentangle pts)

-- |Defunctionalize a program in the Entangled Fragment
-- Fails when not in the fragment
defunc :: [PT] -> Maybe [PT]
defunc pts
    | any hasIllegalRules pts = Nothing
    | otherwise = defuncLegal pts