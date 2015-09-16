module UroboroTransformations.CopatternCoverage.CCTree (
    CCTree (Leaf, VarSplit, ResSplit)
  , PTSig
  , possibleTrees
  , splittingDepth
  , lowestSubtree
  , cutoffLowestSubtree
  , isSimpleTree
  , leaves
  , BetterProgram (ts, cs, ds, fs, rs)
  , betterProgram
) where

import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Maybe
import Data.List (find)
import Data.Set (fromList)
import Debug.Trace

import Uroboro.Checker
import Uroboro.Tree
import Uroboro.Error

import UroboroTransformations.Util
import UroboroTransformations.Util.Typed

data CCTree = VarSplit TQ PathToSubterm [CCTree] | ResSplit TQ [CCTree] | Leaf TQ deriving (Show)

convertToTypedVar :: Type -> State Int TP
convertToTypedVar t = do
    n <- get
    modify (+1)
    return $ TPVar t ("x"++(show n))

tqForDes :: TQ -> PTDes -> TQ
tqForDes tq (PTDes l t id ts _) = TQDes t id (evalState (mapM convertToTypedVar ts) (newvarIndex tq)) tq

ptConToTP :: Int -> PTCon -> TP
ptConToTP n (PTCon _ t id ts) = TPCon t id (evalState (mapM convertToTypedVar ts) n)

splitVarTPs :: [TP] -> Identifier -> PathToSubterm -> Reader (BetterProgram, Int) ([[TP]], PathToSubterm)
splitVarTPs ((tp@(TPVar t id)):tps) id' p
  | id == id' = do
    ((BetterProgram _ cs _ _ _), n) <- ask
    return ((map (:tps) (map (ptConToTP n) (fromJust (lookup t cs)))), p)
  | otherwise = liftM (first (liftM (tp:))) (splitVarTPs tps id' (nextOnSameLevel p))
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
  return $ runReader (mapM (splitVar [] tq) vs) (bp, (newvarIndex tq))

splitRes :: TQ -> Reader BetterProgram [TQ]
splitRes tq = do
    BetterProgram _ _ ds _ _ <- ask
    return $ map (tqForDes tq) (fromJust (lookup (getType tq) ds))

possibleVarSplitTrees :: Int -> TQ -> ([TQ], PathToSubterm) -> Reader BetterProgram [CCTree]
possibleVarSplitTrees d tq (tqs, p) = do
    trees <- mapM (possibleTreesWithRoot (d-1)) tqs
    return $ map (VarSplit tq p) (sequence trees)

possibleTreesWithRoot :: Int -> TQ -> Reader BetterProgram [CCTree]
possibleTreesWithRoot 0 tq = return [Leaf tq]
possibleTreesWithRoot d tq = do
    tqs1 <- splitRes tq
    trees1 <- mapM (possibleTreesWithRoot (d-1)) tqs1
    tqss <- splitVars tq
    trees2 <- mapM (possibleVarSplitTrees d tq) tqss
    return $ (Leaf tq):((map (ResSplit tq) (sequence trees1))++(concat trees2))

headTQ :: PTSig -> TQ
headTQ (id, (_, ts, t)) = TQApp t id (evalState (mapM convertToTypedVar ts) 0)

possibleTreesBProg :: PTSig -> Int -> Reader BetterProgram [CCTree]
possibleTreesBProg sig d = possibleTreesWithRoot d (headTQ sig)

possibleTrees :: PTSig -> Int -> Reader Program [CCTree]
possibleTrees sig d = withReader betterProgram (possibleTreesBProg sig d)

splittingDepthPP :: PP -> Int
splittingDepthPP (PPVar _ _) = 0
splittingDepthPP (PPCon _ _ pps) = 1 + (sum $ map splittingDepthPP pps)

splittingDepth :: PQ -> Int
splittingDepth (PQApp _ _ pps) = sum $ map splittingDepthPP pps
splittingDepth (PQDes _ _ pps pq) = 1 + (sum $ map splittingDepthPP pps) + (splittingDepth pq)

leaves :: CCTree -> [TQ]
leaves (Leaf tq) = [tq]
leaves (ResSplit _ trees) = concatMap leaves trees
leaves (VarSplit _ _ trees) = concatMap leaves trees

data SubtreeMode = Initial | NonInitial

isLeaf :: CCTree -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False

children :: CCTree -> [CCTree]
children (ResSplit _ ts) = ts
children (VarSplit _ _ ts) = ts

lowestSubtrees :: CCTree -> SubtreeMode -> [CCTree]
lowestSubtrees t@(Leaf tq) Initial = [t]
lowestSubtrees (Leaf _) NonInitial = []
lowestSubtrees t _
  | all isLeaf (children t) = [t]
  | otherwise = concatMap (flip lowestSubtrees NonInitial) (children t)

lowestSubtree :: CCTree -> CCTree
lowestSubtree t = (lowestSubtrees t Initial) !! 0

lowestSubtreeToLeaf :: [CCTree] -> [CCTree]
lowestSubtreeToLeaf [] = []
lowestSubtreeToLeaf ((t@(Leaf _)):ts) = t:(lowestSubtreeToLeaf ts)
lowestSubtreeToLeaf (t:ts)
  | all isLeaf (children t) = (Leaf (getTQ t)):ts
  | otherwise = (cutoffLowestSubtree t):ts
  where
    getTQ (ResSplit tq _) = tq
    getTQ (VarSplit tq _ _) = tq

cutoffLowestSubtree :: CCTree -> CCTree
cutoffLowestSubtree (ResSplit tq ts)
  | all isLeaf ts = Leaf tq
  | otherwise = ResSplit tq (lowestSubtreeToLeaf ts)
cutoffLowestSubtree (VarSplit tq p ts)
  | all isLeaf ts = Leaf tq
  | otherwise = VarSplit tq p (lowestSubtreeToLeaf ts)

isSimpleTree :: CCTree -> Bool
isSimpleTree (Leaf _) = True
isSimpleTree (ResSplit _ ts) = all isLeaf ts
isSimpleTree (VarSplit _ _ ts) = all isLeaf ts
