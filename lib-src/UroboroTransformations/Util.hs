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

data BetterProgram = BetterProgram {
      ts :: [Type]
    , cs :: [(Type, [PTCon])]
    , ds :: [(Type, [PTDes])]
    , fs :: [PTSig]
    , rs :: Rules
}

class Typed a where
  getType :: a -> Type

instance Typed PTCon where
  getType (PTCon _ t _ _) = t

instance Typed PTDes where
  getType (PTDes _ _ _ _ t) = t

instance Typed TQ where
  getType (TQApp t _ _) = t
  getType (TQDes t _ _ _) = t

hasType :: Typed a => Type -> a -> Bool
hasType t tpd = t == (getType tpd)

betterProgram :: Program -> BetterProgram
betterProgram (Program ts cs ds fs rs) = BetterProgram ts (assocWithType cs) (assocWithType ds) fs rs
  where
    assocWithType xs = [(t, filter (hasType t) xs) | t <- ts]

type PTSig = (Identifier, (Location, [Type], Type))

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

instance Eq PTCon where
  (PTCon _ t id ts) == (PTCon _ t' id' ts') = (t == t') && (id == id') && (ts == ts')

instance Eq PTDes where
  (PTDes _ t id ts dt) == (PTDes _ t' id' ts' dt') = (t == t') && (id == id') && (ts == ts') && (dt == dt')

-- used for tests
instance Ord Type where
  t <= t' = (show t) <= (show t')

-- used for tests
instance Ord PTCon where
  (PTCon _ _ id _) <= (PTCon _ _ id' _) = id <= id'

-- used for tests
instance Ord PTDes where
  (PTDes _ _ id _ _) <= (PTDes _ _ id' _ _) = id <= id'

instance Eq PT where
  (PTFun _ id ts t rs) == (PTFun _ id' ts' t' rs') = (id == id') && (ts == ts') && (t == t') && ((fromList rs) == (fromList rs'))
  (PTPos _ t cs) == (PTPos _ t' cs') = (t == t') && ((fromList cs) == (fromList cs'))
  (PTNeg _ t ds) == (PTNeg _ t' ds') = (t == t') && ((fromList ds) == (fromList ds'))
  _ == _ = False

forgetLocationAndVarNames :: PQ -> PQ
forgetLocationAndVarNames (PQDes _ id pps pq) = PQDes dummyLocation id (map forgetInPP pps) (forgetLocationAndVarNames pq)
forgetLocationAndVarNames (PQApp _ id pps) = PQApp dummyLocation id (map forgetInPP pps)

forgetInPP :: PP -> PP
forgetInPP (PPVar _ _) = PPVar dummyLocation ""
forgetInPP (PPCon _ id pps) = PPCon dummyLocation id (map forgetInPP pps)

instance Ord PQ where
  (<=) = curry ((uncurry (<=)) . (join (***) (show . forgetLocationAndVarNames)))

-- used for tests
instance Ord PTRule where
  (PTRule _ pq _) <= (PTRule _ pq' _) = pq <= pq'

-- used for tests
instance Ord PT where
  (PTFun _ id _ _ rs) <= (PTFun _ id' _ _ rs') = id <= id'
  (PTFun _ id _ _ _) <= (PTPos _ t' _) = id <= (show t')
  (PTFun _ id _ _ _) <= (PTNeg _ t' _) = id <= (show t')
  (PTPos _ t _) <= (PTFun _ id' _ _ _) = (show t) <= id'
  (PTPos _ t _) <= (PTPos _ t' _) = t <= t'
  (PTPos _ t _) <= (PTNeg _ t' _) = t <= t'
  (PTNeg _ t _) <= (PTFun _ id' _ _ _) = (show t) <= id'
  (PTNeg _ t _) <= (PTPos _ t' _) = t <= t'
  (PTNeg _ t _) <= (PTNeg _ t' _) = t <= t'

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

isFun :: PT -> Bool
isFun (PTFun _ _ _ _ _) = True
isFun _ = False

funSigs :: [PT] -> [(Location, Identifier, [Type], Type)]
funSigs = (map sig) . (filter isFun)
  where
    sig (PTFun l id ts t _) = (l, id, ts, t)
    sig _ = undefined

-- |Retrieves all rules from the parse trees.
-- |Fails when a rule is illegal according to the supplied predicate.
funRulesLegal :: [PT] -> (PTRule -> Bool) -> Maybe [[PTRule]]
funRulesLegal pts ill = (mapM rules (filter isFun pts))
  where
    rules (PTFun _ _ _ _ rs)
        | any ill rs = Nothing
        | otherwise          = Just rs
    rules _ = undefined
