module UroboroTransformations.FragmentsForDefunc.Entangled.Defunc where

import Uroboro.Tree
import Uroboro.Error

import UroboroTransformations.Util
import UroboroTransformations.Util.HelperFuns
import UroboroTransformations.FragmentsForDefunc.Entangled.FragmentTest

import qualified UroboroTransformations.CoDataDefsDisj.Defunc as CoDataDefsDisjD

import Data.List(nubBy, groupBy)
import Data.Monoid
import Control.Monad.State.Lazy
import Control.Monad.Trans.Writer.Lazy

ruleToHelperFuns :: PT -> [Type] -> Type -> Maybe PTRule -> HelperFuns
ruleToHelperFuns fun desTs desRT (Just r@(PTRule l (PQApp _ id _) _)) =
    makeHelperFuns $ (PTFun l id ((funArgTypes fun) ++ desTs) desRT [r])
  where
    funArgTypes (PTFun _ _ ts _ _) = ts
ruleToHelperFuns _ _ _ Nothing = mempty
ruleToHelperFuns _ _ _ _ = undefined

helperFunRule :: Identifier -> PTRule -> Maybe PTRule
helperFunRule id (PTRule l (PQDes _ _ pps (PQApp l' _ pps')) e) =
    Just $ PTRule l (PQApp l' id (pps' ++ pps)) e
helperFunRule _ _ = Nothing

extractPatternMatching :: [PT] -> PT -> PTRule -> Writer HelperFuns PTRule
extractPatternMatching pts fun r@(PTRule l (PQDes l' id pps (PQApp l'' id' pps')) e)
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
extractPatternMatching _ _ r = return r

disentangle :: [PT] -> [PT]
disentangle = extractHelperFuns extractPatternMatching

defuncLegal :: [PT] -> Maybe [PT]
defuncLegal pts = CoDataDefsDisjD.defuncLegal (disentangle pts)

-- |Defunctionalize a program in the Entangled Fragment
-- Fails when not in the fragment
defunc :: [PT] -> Maybe [PT]
defunc pts
    | any hasIllegalRules pts = Nothing
    | otherwise = defuncLegal pts