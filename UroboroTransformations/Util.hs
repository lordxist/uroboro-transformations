module UroboroTransformations.Util where

import Uroboro.Tree
import Uroboro.Error

-- At the moment, locations aren't correctly stored after transformations anyway
dummyLocation :: Location
dummyLocation = MakeLocation "" (-1) (-1)

toExpr :: PP -> PExp
toExpr (PPVar l id) = PVar l id
toExpr (PPCon l id pps) = PApp l id (map toExpr pps)

conIdentifier :: PTCon -> Identifier
conIdentifier (PTCon _ _ id _) = id

constructors :: PT -> [PTCon]
constructors (PTPos _ _ cons) = cons
constructors _ = []

desIdentifier :: PTDes -> Identifier
desIdentifier (PTDes _ _ id _ _) = id

destructors :: PT -> [PTDes]
destructors (PTNeg _ _ dess) = dess
destructors _ = []

funIdentifier :: PT -> Identifier
funIdentifier (PTFun _ id _ _ _) = id
funIdentifier _ = undefined

gensym :: Identifier -> Identifier -> [PT] -> Identifier
gensym id1 id2 pts = (findUnusedIdPrefix 0) ++ "_" ++ id1 ++ "_" ++ id2
  where
    findUnusedIdPrefix n
        | not (pref `elem` usedPrefixes) = pref
        | otherwise = findUnusedIdPrefix (n+1)
        where pref = "autogen" ++ (show n)

    usedPrefixes = (map desIdentifier (concatMap destructors pts)) ++
                    (map conIdentifier (concatMap constructors pts)) ++
                    (map funIdentifier (filter isFunctionDef pts))

    isFunctionDef (PTFun _ _ _ _ _) = True
    isFunctionDef _ = False