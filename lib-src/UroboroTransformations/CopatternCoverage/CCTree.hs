module UroboroTransformations.CopatternCoverage.CCTree where

import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Maybe
import Data.List (find)
import Data.Set (fromList)

import Uroboro.Checker
import Uroboro.Tree
import Uroboro.Error

import UroboroTransformations.Util (dummyLocation)

data Tree a = Branch a [Tree a] | Leaf a deriving (Show)

type PTSig = (Identifier, (Location, [Type], Type))

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
  getType (PTDes _ t _ _ _) = t

instance Typed TQ where
  getType (TQApp t _ _) = t
  getType (TQDes t _ _ _) = t

hasType :: Typed a => Type -> a -> Bool
hasType t tpd = t == (getType tpd)

betterProgram :: Program -> BetterProgram
betterProgram (Program ts cs ds fs rs) = BetterProgram ts (assocWithType cs) (assocWithType ds) fs rs
  where
    assocWithType xs = [(t, filter (hasType t) xs) | t <- ts]

convertToVar :: Type -> State Int TP
convertToVar t = do
    n <- get
    modify (+1)
    return $ TPVar t ("x"++(show n))

tqForDes :: TQ -> PTDes -> TQ
tqForDes tq (PTDes l t id ts _) = TQDes t id (evalState (mapM convertToVar ts) 0) tq

containsVarTP :: Identifier -> TP -> Bool
containsVarTP id' (TPVar _ id) = id == id'
containsVarTP id (TPCon _ _ tps) = any (containsVarTP id) tps

ptConToTP :: PTCon -> TP
ptConToTP (PTCon _ t id ts) = TPCon t id (evalState (mapM convertToVar ts) 0)

splitVarTPs :: [TP] -> Identifier -> Reader BetterProgram [[TP]]
splitVarTPs ((tp@(TPVar t id)):tps) id'
  | id == id' = do
    BetterProgram _ cs _ _ _ <- ask
    return $ map (:tps) (map ptConToTP (fromJust (lookup t cs)))
  | otherwise = liftM (liftM (tp:)) (splitVarTPs tps id)
splitVarTPs ((TPCon l cid tps):tps') id
  | any (containsVarTP id) tps = liftM (liftM $ ((:tps').(TPCon l cid))) (splitVarTPs tps id)
  | otherwise = liftM (liftM $ ((TPCon l cid tps):)) (splitVarTPs tps' id)

containsVar :: TQ -> Identifier -> Bool
containsVar (TQDes _ _ tps tq) id = (any (containsVarTP id) tps) || (containsVar tq id)
containsVar (TQApp _ _ tps) id = any (containsVarTP id) tps

splitVar :: TQ -> Identifier -> Reader BetterProgram [TQ]
splitVar (TQDes l id' tps tq) id
  | tq `containsVar` id = liftM (liftM $ TQDes l id' tps) (splitVar tq id)
  | otherwise = liftM (liftM $ flip (TQDes l id') tq) (splitVarTPs tps id)
splitVar (TQApp l id' tps) id = liftM (liftM $ TQApp l id') (splitVarTPs tps id)

varsTP :: TP -> [Identifier]
varsTP (TPVar _ id) = [id]
varsTP _ = []

vars :: TQ -> [Identifier]
vars (TQDes _ _ tps tq) = (vars tq) ++ (concatMap varsTP tps)
vars (TQApp _ _ tps) = (concatMap varsTP tps)

splitVars :: TQ -> Reader BetterProgram [TQ]
splitVars tq = (liftM concat) $ mapM (splitVar tq) (vars tq)

splitRes :: TQ -> Reader BetterProgram [TQ]
splitRes tq = do
    BetterProgram _ _ ds _ _ <- ask
    return $ map (tqForDes tq) (fromJust (lookup (getType tq) ds))

possibleTreesWithRoot :: Int -> TQ -> Reader BetterProgram [Tree TQ]
possibleTreesWithRoot 0 tq = return [Leaf tq]
possibleTreesWithRoot d tq = do
    tqs1 <- splitRes tq
    trees1 <- mapM (possibleTreesWithRoot (d-1)) tqs1
    tqs2 <- splitVars tq
    trees2 <- mapM (possibleTreesWithRoot (d-1)) tqs2
    return $ (Leaf tq):(map (Branch tq) ((sequence trees1)++(sequence trees2)))

headTQ :: PTSig -> TQ
headTQ (id, (_, ts, t)) = TQApp t id (evalState (mapM convertToVar ts) 0)

possibleTreesBProg :: PTSig -> Int -> Reader BetterProgram [Tree TQ]
possibleTreesBProg sig d = possibleTreesWithRoot d (headTQ sig)

possibleTrees :: PTSig -> Int -> Reader Program [Tree TQ]
possibleTrees sig d = withReader betterProgram (possibleTreesBProg sig d)

splittingDepthPP :: PP -> Int
splittingDepthPP (PPVar _ _) = 0
splittingDepthPP (PPCon _ _ pps) = 1 + (sum $ map splittingDepthPP pps)

splittingDepth :: PQ -> Int
splittingDepth (PQApp _ _ pps) = sum $ map splittingDepthPP pps
splittingDepth (PQDes _ _ pps pq) = 1 + (sum $ map splittingDepthPP pps) + (splittingDepth pq)

instance Ord PQ where
  pq <= pq' = (splittingDepth pq) <= (splittingDepth pq')

toPP :: TP -> PP
toPP (TPVar _ id) = PPVar dummyLocation id
toPP (TPCon _ id tps) = PPCon dummyLocation id (map toPP tps)

toPQ :: TQ -> PQ
toPQ (TQApp _ id tps) = PQApp dummyLocation id (map toPP tps)
toPQ (TQDes _ id tps tq) = PQDes dummyLocation id (map toPP tps) (toPQ tq)

leaves :: Tree TQ -> [TQ]
leaves (Leaf tq) = [tq]
leaves (Branch _ trees) = concatMap leaves trees

instance Eq PP where
  (PPVar _ id) == (PPVar _ id') = id == id'
  (PPCon _ id pps) == (PPCon _ id' pps') = (id == id') && (pps == pps')
  _ == _ = False

instance Eq PQ where
  (PQApp _ id pps) == (PQApp _ id' pps') = (id == id') && (pps == pps')
  (PQDes _ id pps pq) == (PQDes _ id' pps' pq') = (id == id') && (pps == pps') && (pq == pq')
  _ == _ = False

leavesEqualPQs :: [PQ] -> Tree TQ -> Bool
leavesEqualPQs pqs tree = (fromList (map toPQ (leaves tree))) == (fromList pqs)

checkCoverage :: PTSig -> PT -> Reader Program (Maybe (Tree TQ))
checkCoverage sig (PTFun _ _ _ _ rs) = (liftM $ find (leavesEqualPQs (map lhs rs))) searchSpace
  where
    searchSpace = possibleTrees sig (maximum $ map (splittingDepth . lhs) rs)

    lhs (PTRule _ pq _) = pq

zipCoverageRules :: [TQ] -> [PTRule] -> [(PTRule, TQ)]
zipCoverageRules [] _ = []
zipCoverageRules (tq:tqs) rs
    = ((fromJust $ find fitsWithTQ rs),tq):(zipCoverageRules tqs rs)
  where
    fitsWithTQ (PTRule _ pq _) = (toPQ tq) == pq