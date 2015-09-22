module UroboroTransformations.Util where

import Uroboro.Checker
import Uroboro.Tree
import Uroboro.Error

import Data.Char
import Data.List(intercalate, isPrefixOf, maximumBy)
import Data.Set(fromList)
import Control.Arrow
import Control.Monad
import Control.Monad.State.Lazy

import UroboroTransformations.Util.Typed
import UroboroTransformations.Util.Equality

-- | Suppress the 'Left' value of an 'Either'.
-- Stolen from Control.Error.Util (errors-2.0.0).
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

type PathToSubterm = [Int]

forgetLocationAndVarNames :: PQ -> PQ
forgetLocationAndVarNames (PQDes _ id pps pq) = PQDes dummyLocation id (map forgetInPP pps) (forgetLocationAndVarNames pq)
forgetLocationAndVarNames (PQApp _ id pps) = PQApp dummyLocation id (map forgetInPP pps)

forgetInPP :: PP -> PP
forgetInPP (PPVar _ _) = PPVar dummyLocation ""
forgetInPP (PPCon _ id pps) = PPCon dummyLocation id (map forgetInPP pps)

instance Ord PQ where
  (<=) = curry ((uncurry (<=)) . (join (***) (show . forgetLocationAndVarNames)))

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

isFun :: PT -> Bool
isFun (PTFun _ _ _ _ _) = True
isFun _ = False

funSigs :: [PT] -> [(Location, Identifier, [Type], Type)]
funSigs = (map sig) . (filter isFun)
  where
    sig (PTFun l id ts t _) = (l, id, ts, t)
    sig _ = undefined

-- |Retrieves all rules from the parse trees.
-- Fails when a rule is illegal according to the supplied predicate.
funRulesLegal :: [PT] -> (PTRule -> Bool) -> Maybe [[PTRule]]
funRulesLegal pts ill = (mapM rules (filter isFun pts))
  where
    rules (PTFun _ _ _ _ rs)
        | any ill rs = Nothing
        | otherwise          = Just rs
    rules _ = undefined
