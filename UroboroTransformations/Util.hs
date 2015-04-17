module UroboroTransformations.Util where

import Uroboro.Tree
import Uroboro.Error

import Data.List(intercalate)

-- At the moment, locations aren't correctly stored after transformations anyway
dummyLocation :: Location
dummyLocation = MakeLocation "" (-1) (-1)

toExpr :: PP -> PExp
toExpr (PPVar l id) = PVar l id
toExpr (PPCon l id pps) = PApp l id (map toExpr pps)

con :: PP -> Bool
con (PPCon _ _ _) = True
con _             = False

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

-- Problem: not a unique mapping. Can be made to be one, but ideally the names should then
-- be simplified to a nicer form whenever there are no conflicts.
namePattern :: PQ -> String
namePattern (PQApp _ id pps) = id ++ "_" ++ (intercalate "__" (map namePPPattern pps))
namePattern (PQDes _ id pps pq) =
    id ++ "_" ++ (intercalate "__" (map namePPPattern pps)) ++ "__" ++ (namePattern pq)

namePPPattern :: PP -> String
namePPPattern (PPVar _ _) = ""
namePPPattern (PPCon _ id pps) = id ++ "_" ++ (intercalate "__" (map namePPPattern pps))

collectVars :: PP -> [PP]
collectVars (PPCon _ _ pps) = concatMap collectVars pps
collectVars v = [v]

collectVarsPQ :: PQ -> [PP]
collectVarsPQ (PQDes _ _ pps pq') = (collectVarsPQ pq') ++ (concatMap collectVars pps)
collectVarsPQ (PQApp _ _ pps)     = concatMap collectVars pps

-- Requires globally unique constructor names. Otherwise collectVarTypes needs to be implemented
-- using typed syntax trees (and the transformation is run on the typecheck result).
constructorTypes :: Identifier -> [PT] -> [Type]
constructorTypes id pts = types ((concatMap getConstructorForId pts) !! 0)
  where
    getConstructorForId (PTPos l t ((con@(PTCon _ _ id' _)):cons))
        | id' == id = [con]
        | otherwise = getConstructorForId (PTPos l t cons)
    getConstructorForId _ = []

    types (PTCon _ _ _ ts) = ts

getDestructorForId :: Identifier -> PT -> [PTDes]
getDestructorForId id (PTNeg l t ((des@(PTDes _ _ id' _ _)):dess))
        | id' == id = [des]
        | otherwise = getDestructorForId id (PTNeg l t dess)
getDestructorForId id _ = []

-- Analogous to constructorTypes.
destructorTypes :: Identifier -> [PT] -> [Type]
destructorTypes id pts = types ((concatMap (getDestructorForId id) pts) !! 0)
  where
    types (PTDes _ _ _ ts _) = ts

-- Analogous limitation to constructorTypes and destructorTypes.
destructorReturnType :: Identifier -> [PT] -> Type
destructorReturnType id pts = returnType ((concatMap (getDestructorForId id) pts) !! 0)
  where
    returnType (PTDes _ t _ _ _) = t

varTypes :: [PT] -> (PP, Type) -> [Type]
varTypes pts ((PPVar _ _), t)       = [t]
varTypes pts ((PPCon _ id pps'), _) = concatMap (varTypes pts) (zip pps' (constructorTypes id pts))

collectVarTypes :: [PT] -> [Type] -> PQ -> [Type]
collectVarTypes pts ts (PQApp _ _ pps) = concatMap (varTypes pts) (zip pps ts)
collectVarTypes pts ts (PQDes _ id pps pq) =
    (collectVarTypes pts ts pq) ++ (concatMap (varTypes pts) (zip pps (destructorTypes id pts)))    