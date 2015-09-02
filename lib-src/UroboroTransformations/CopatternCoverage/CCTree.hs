module UroboroTransformations.CopatternCoverage.CCTree where

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Maybe
import Data.List (find)
import Data.Set (fromList)
import Debug.Trace

import Uroboro.Checker
import Uroboro.Tree
import Uroboro.Error

import UroboroTransformations.Util (dummyLocation, PathToSubterm, nextOnSameLevel, largestVarIndex, collectVarsTQ, containsVar, containsVarTP)

data CCTree a = VarSplit a PathToSubterm [CCTree a] | ResSplit a [CCTree a] | Leaf a deriving (Show)

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

convertToVar :: Type -> State Int TP
convertToVar t = do
    n <- get
    modify (+1)
    return $ TPVar t ("x"++(show n))

tqForDes :: TQ -> PTDes -> TQ
tqForDes tq (PTDes l t id ts _) = TQDes t id (evalState (mapM convertToVar ts) 0) tq

ptConToTP :: Int -> PTCon -> TP
ptConToTP n (PTCon _ t id ts) = TPCon t id (evalState (mapM convertToVar ts) n)

splitVarTPs :: [TP] -> Identifier -> PathToSubterm -> Reader (BetterProgram, Int) ([[TP]], PathToSubterm)
splitVarTPs ((tp@(TPVar t id)):tps) id' p
  | id == id' = do
    ((BetterProgram _ cs _ _ _), n) <- ask
    return ((map (:tps) (map (ptConToTP n) (fromJust (lookup t cs)))), p)
  | otherwise = liftM (first (liftM (tp:))) (splitVarTPs tps id (nextOnSameLevel p))
splitVarTPs ((TPCon l cid tps):tps') id p
  | any (containsVarTP id) tps = liftM (first $ liftM ((:tps').(TPCon l cid))) (splitVarTPs tps id (p++[0]))
  | otherwise = liftM (first $ liftM ((TPCon l cid tps):)) (splitVarTPs tps' id (nextOnSameLevel p))
splitVarTPs _ id p = error (show id)

splitVar :: PathToSubterm -> TQ -> Identifier -> Reader (BetterProgram, Int) ([TQ], PathToSubterm)
splitVar p (TQDes l id' tps tq) id
  | tq `containsVar` id = liftM (first $ liftM $ TQDes l id' tps) (splitVar (p++[0]) tq id)
  | otherwise = liftM (first $ liftM $ flip (TQDes l id') tq) (splitVarTPs tps id (p++[1]))
splitVar p (TQApp l id' tps) id = liftM (first $ liftM $ TQApp l id') (splitVarTPs tps id (p++[0]))

tqPosVarIds :: TQ -> BetterProgram -> [Identifier]
tqPosVarIds tq bp = map getId (filter positive (collectVarsTQ tq))
  where
    getId (TPVar _ id) = id

    positive (TPVar t _) = (null $ fromJust $ lookup t (ds bp)) -- warning: wrongly judges a type to be positive when it is the empty codata type

splitVars :: TQ -> Reader BetterProgram [([TQ], PathToSubterm)]
splitVars tq = do
  bp <- ask
  let vs = tqPosVarIds tq bp
  return $ runReader (mapM (splitVar [] tq) vs) (bp, ((largestVarIndex vs)+1))

splitRes :: TQ -> Reader BetterProgram [TQ]
splitRes tq = do
    BetterProgram _ _ ds _ _ <- ask
    return $ map (tqForDes tq) (fromJust (lookup (getType tq) ds))

possibleVarSplitTrees :: Int -> TQ -> ([TQ], PathToSubterm) -> Reader BetterProgram [CCTree TQ]
possibleVarSplitTrees d tq (tqs, p) = do
    trees <- mapM (possibleTreesWithRoot (d-1)) tqs
    return $ map (VarSplit tq p) (sequence trees)

possibleTreesWithRoot :: Int -> TQ -> Reader BetterProgram [CCTree TQ]
possibleTreesWithRoot 0 tq = return [Leaf tq]
possibleTreesWithRoot d tq = do
    tqs1 <- splitRes tq
    trees1 <- mapM (possibleTreesWithRoot (d-1)) tqs1
    tqss <- splitVars tq
    trees2 <- mapM (possibleVarSplitTrees d tq) tqss
    return $ (Leaf tq):((map (ResSplit tq) (sequence trees1))++(concat trees2))

headTQ :: PTSig -> TQ
headTQ (id, (_, ts, t)) = TQApp t id (evalState (mapM convertToVar ts) 0)

possibleTreesBProg :: PTSig -> Int -> Reader BetterProgram [CCTree TQ]
possibleTreesBProg sig d = possibleTreesWithRoot d (headTQ sig)

possibleTrees :: PTSig -> Int -> Reader Program [CCTree TQ]
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

leaves :: CCTree TQ -> [TQ]
leaves (Leaf tq) = [tq]
leaves (ResSplit _ trees) = concatMap leaves trees
leaves (VarSplit _ _ trees) = concatMap leaves trees

instance Eq PP where
  (PPVar _ id) == (PPVar _ id') = id == id'
  (PPCon _ id pps) == (PPCon _ id' pps') = (id == id') && (pps == pps')
  _ == _ = False

instance Eq PQ where
  (PQApp _ id pps) == (PQApp _ id' pps') = (id == id') && (pps == pps')
  (PQDes _ id pps pq) == (PQDes _ id' pps' pq') = (id == id') && (pps == pps') && (pq == pq')
  _ == _ = False

leavesEqualPQs :: [PQ] -> CCTree TQ -> Bool
leavesEqualPQs pqs tree = (fromList (map toPQ (leaves tree))) == (fromList pqs)

checkCoverage :: PTSig -> PT -> Reader Program (Maybe (CCTree TQ))
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

data SubtreeMode = Initial | NonInitial

lowestSubtrees :: CCTree TQ -> SubtreeMode -> [CCTree TQ]
lowestSubtrees t@(Leaf tq) Initial = [t]
lowestSubtrees (Leaf _) NonInitial = []
lowestSubtrees t _
  | all isLeaf (children t) = [t]
  | otherwise = concatMap (flip lowestSubtrees NonInitial) (children t)
    where
      isLeaf (Leaf _) = True
      isLeaf _ = False

      children (ResSplit _ ts) = ts
      children (VarSplit _ _ ts) = ts

lowestSubtree :: CCTree TQ -> CCTree TQ
lowestSubtree t = (lowestSubtrees t Initial) !! 0
