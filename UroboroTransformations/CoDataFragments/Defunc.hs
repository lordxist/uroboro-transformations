module UroboroTransformations.CoDataFragments.Defunc where

import Uroboro.Tree
import Uroboro.Error

import UroboroTransformations.CoDataFragments

defuncExp :: PExp -> PExp
defuncExp v@(PVar _ _) = v
defuncExp (PApp l id es) = PApp l id (map defuncExp es)
defuncExp (PDes l id es e) = PApp l id (map defuncExp (e:es))

illegalRule :: PTRule -> Bool
illegalRule (PTRule _ (PQApp l _ _) _) = True
illegalRule (PTRule _ (PQDes _ _ pps pq) _) = (any con pps) || (illegalPQ pq)
  where
    con (PPCon l _ _) = True
    con _             = False

    illegalPQ (PQDes l _ _ _) = True
    illegalPQ (PQApp _ _ pps)   = any con pps

defuncFunSig :: (Location, Identifier, [Type], Type) -> PTCon
defuncFunSig (l, id, ts, t) = PTCon l t id ts

defuncDef :: [(Location, Identifier, [Type], Type)] -> PT -> Maybe PT
defuncDef _ (PTPos _ _ _)  = Nothing
defuncDef fs (PTNeg l t _) = Just $ PTPos l t (map defuncFunSig (filter (hasSameType) fs))
  where
    hasSameType (_, _, _, t') = t' == t
defuncDef _ (PTFun _ _ _ _ _) = undefined

defuncRule :: PTRule -> PTRule
defuncRule (PTRule l (PQDes l' id pps (PQApp l'' id' pps')) e) =
    PTRule l (PQApp l' id ((PPCon l'' id' pps'):pps)) (defuncExp e)

defuncRules :: Identifier -> [PTRule] -> [PTRule]
defuncRules id rs = map defuncRule (filter (hasId id) rs)
  where
    hasId id (PTRule _ (PQDes _ id' _ _) _) = id == id'
    hasId _ _ = undefined

funForDes :: [PTRule] -> PTDes -> PT
funForDes rs (PTDes l t id ts t') = PTFun l id (t':ts) t (defuncRules id rs)

-- |Defunctionalize a program in the Codata Fragment
-- Fails when not in the fragment
defunc :: [PT] -> Maybe [PT]
defunc pts = do
    cs <- mapM (defuncDef (funSigs pts)) (filter (not . isFun) pts)
    rs <- funRules pts illegalRule
    let fs = map (funForDes (concat rs)) (concatMap dess pts)
    return $ cs ++ fs
  where
    dess (PTNeg _ _ ds) = ds
    dess _ = []