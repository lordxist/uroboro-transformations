module UroboroTransformations.Unnest.ForDefunc where

import UroboroTransformations.Unnest
import UroboroTransformations.Util

import Uroboro.Tree

isDNestedFunDef :: PT -> Bool
isDNestedFunDef (PTFun _ _ _ _ rs) = any isDNestedRule rs
  where
    isDNestedRule (PTRule _ (PQDes _ _ _ (PQDes _ _ _ _)) _) = True
    isDNestedRule (PTRule _ (PQDes _ _ pps' (PQApp _ _ pps)) _) = any con (pps++pps')
    isDNestedRule _ = False
isDNestedFunDef _ = False

unnestForDefunc :: [PT] -> Maybe [PT]
unnestForDefunc = unnestFor isDNestedFunDef
