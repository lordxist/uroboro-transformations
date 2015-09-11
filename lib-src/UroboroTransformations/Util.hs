module UroboroTransformations.Util where

import Uroboro.Checker
import Uroboro.Tree
import Uroboro.Error

import Data.Char
import Data.List(intercalate, isPrefixOf, maximumBy)
import Control.Monad.State.Lazy

betterTypecheck :: [PT] -> Either Error Program
betterTypecheck defs = do
  pre  <- foldM preCheckPT emptyProgram defs
  prog <- foldM postCheckPT pre defs
  return prog

type PathToSubterm = [Int]

instance Eq PP where
  (PPVar _ id) == (PPVar _ id') = True
  (PPCon _ id pps) == (PPCon _ id' pps') = (id == id') && (pps == pps')
  _ == _ = False

instance Eq PQ where
  (PQApp _ id pps) == (PQApp _ id' pps') = (id == id') && (pps == pps')
  (PQDes _ id pps pq) == (PQDes _ id' pps' pq') = (id == id') && (pps == pps') && (pq == pq')
  _ == _ = False

instance Eq PExp where
  (PApp _ id pexps) == (PApp _ id' pexps') = (id == id') && (pexps == pexps')
  (PVar _ id) == (PVar _ id') = id == id'
  (PDes _ id pexps pexp) == (PDes _ id' pexps' pexp') = (id == id') && (pexps == pexps') && (pexp == pexp')
  _ == _ = False

instance Eq PTRule where
  (PTRule _ pq pexp) == (PTRule _ pq' pexp') = (pq == pq') && (pexp == pexp')

rulesForFunDef :: PT -> [PTRule]
rulesForFunDef (PTFun _ _ _ _ rs) = rs

containsVarTP :: Identifier -> TP -> Bool
containsVarTP id' (TPVar _ id) = id == id'
containsVarTP id (TPCon _ _ tps) = any (containsVarTP id) tps

containsVar :: TQ -> Identifier -> Bool
containsVar (TQDes _ _ tps tq) id = (any (containsVarTP id) tps) || (containsVar tq id)
containsVar (TQApp _ _ tps) id = any (containsVarTP id) tps

largestVarIndex :: [Identifier] -> Int
largestVarIndex ids
    | null xSchemeIds = 0
    | otherwise = getIndex $ maximumBy maxInXScheme xSchemeIds
  where
    xSchemeIds = filter hasXScheme ids

    hasXScheme ('x':rs) = ((not.null) rs) && (all isDigit rs)
    hasXScheme _ = False

    maxInXScheme ('x':rs) ('x':rs') = (compare :: Int -> Int -> Ordering) (read rs) (read rs')

    getIndex ('x':rs) = read rs

tqVarIds :: TQ -> [Identifier]
tqVarIds tq = map getId (collectVarsTQ tq)
  where
    getId (TPVar _ id) = id

newvarIndex :: TQ -> Int
newvarIndex tq = (largestVarIndex $ tqVarIds tq) + 1

nextOnSameLevel :: PathToSubterm -> PathToSubterm
nextOnSameLevel p = reverse $ ((last p)+1):(reverse $ init p)

-- At the moment, locations aren't correctly stored after transformations anyway
dummyLocation :: Location
dummyLocation = MakeLocation "" (-1) (-1)

convertToVar :: a -> State Int PP
convertToVar _ = do
    n <- get
    modify (+1)
    return $ PPVar dummyLocation ("x"++(show n))

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

autogen :: Identifier -> [PT] -> Identifier
autogen id pts = (findUnusedIdPrefix 0) ++ "_" ++ id
  where
    findUnusedIdPrefix n
        | not (any (isPrefixOf pref) usedNames) = pref
        | otherwise = findUnusedIdPrefix (n+1)
        where pref = "autogen" ++ (show n)

    usedNames = (map desIdentifier (concatMap destructors pts)) ++
                    (map conIdentifier (concatMap constructors pts)) ++
                    (map funIdentifier (filter isFunctionDef pts))

    isFunctionDef (PTFun _ _ _ _ _) = True
    isFunctionDef _ = False

collectVars :: PP -> [PP]
collectVars (PPCon _ _ pps) = concatMap collectVars pps
collectVars v = [v]

collectVarsT :: TP -> [TP]
collectVarsT (TPCon _ _ tps) = concatMap collectVarsT tps
collectVarsT v = [v]

collectVarsPQ :: PQ -> [PP]
collectVarsPQ (PQDes _ _ pps pq') = (collectVarsPQ pq') ++ (concatMap collectVars pps)
collectVarsPQ (PQApp _ _ pps)     = concatMap collectVars pps

collectVarsTQ :: TQ -> [TP]
collectVarsTQ (TQDes _ _ tps tq') = (collectVarsTQ tq') ++ (concatMap collectVarsT tps)
collectVarsTQ (TQApp _ _ tps)     = concatMap collectVarsT tps

getDestructorForId :: Identifier -> PT -> [PTDes]
getDestructorForId id (PTNeg l t ((des@(PTDes _ _ id' _ _)):dess))
        | id' == id = [des]
        | otherwise = getDestructorForId id (PTNeg l t dess)
getDestructorForId id _ = []


