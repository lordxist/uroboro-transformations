module UroboroTransformations.Extraction where

import Control.Arrow
import Control.Monad.Reader
import Data.List

import Uroboro.Parser
import Uroboro.Checker
import Uroboro.Tree
import Uroboro.Error

import UroboroTransformations.Util

data ExtractionLens = ExtractionLens {
    -- actually, a TQ with location information is necessary here
      get     :: TQ -> TQ
    , putback :: TQ -> TQ -> TQ
}

data ExtractionSpec = ExtractionSpec {
      lens   :: ExtractionLens
    , target :: [(PTRule, TQ)]
}

type Extraction = Reader ExtractionSpec (PTRule, PT)

typeOfTQ :: TQ -> Type
typeOfTQ (TQApp t _ _) = t
typeOfTQ (TQDes t _ _ _) = t

auxify :: TQ -> TExp
auxify tq = TApp (typeOfTQ tq) "aux" (map tpToTExp (collectVarsTQ tq))
  where
    tpToTExp (TPVar t id) = TVar t id

epsilonLhs :: Reader ExtractionSpec TQ
epsilonLhs = do
    (ExtractionSpec lens target) <- ask
    let (_, tq) = target !! 0
    return $ (get lens) tq

epsilonRhs :: Reader ExtractionSpec TExp
epsilonRhs = liftM auxify epsilonLhs

tpToPP :: TP -> PP
tpToPP (TPVar t id) = PPVar dummyLocation id
tpToPP (TPCon t id tps) = PPCon dummyLocation id (map tpToPP tps)

tqToPQ :: TQ -> PQ
tqToPQ (TQApp t id tps) = PQApp dummyLocation id (map tpToPP tps)
tqToPQ (TQDes t id tps tq) = PQDes dummyLocation id (map tpToPP tps) (tqToPQ tq)    

extractEpsilon :: Reader ExtractionSpec PTRule
extractEpsilon = do
    ExtractionSpec _ (((PTRule l _ _), _):_) <- ask
    lhs <- epsilonLhs
    rhs <- epsilonRhs
    return $ PTRule l (tqToPQ lhs) (tExpToPExp rhs)
  where
    tExpToPExp (TApp t id tExps) = PApp dummyLocation id (map tExpToPExp tExps)
    tExpToPExp (TDes t id tExps tExp) = PDes dummyLocation id (map tExpToPExp tExps) (tExpToPExp tExp)
    tExpToPExp (TVar t id) = PVar dummyLocation id
    tExpToPExp (TCon t id tExps) = PApp dummyLocation id (map tExpToPExp tExps)

extractZetaRules :: Reader ExtractionSpec [PTRule]
extractZetaRules = do
    (ExtractionSpec lens target) <- ask
    lhs <- epsilonRhs
    return $ map (replacePQ . (second (tqToPQ . ((putback lens) (tExpToTQ lhs))))) target
  where
    replacePQ ((PTRule l  _ pexp), pq) = PTRule l pq pexp

    tExpToTQ (TApp t id tExps) = TQApp t id (map tExpToTP tExps)
    tExpToTQ (TDes t id tExps tExp) = TQDes t id (map tExpToTP tExps) (tExpToTQ tExp)

    tExpToTP (TVar t id) = TPVar t id
    tExpToTP (TCon t id tExps) = TPCon t id (map tExpToTP tExps)

extractZetaSig :: Reader ExtractionSpec ([PTRule] -> PT)
extractZetaSig = do
    texp <- epsilonRhs
    return $ PTFun dummyLocation "aux" (argTs texp) (resT texp)
  where
    argTs (TApp _ _ texps) = map varT texps

    varT (TVar t _) = t

    resT (TApp t _ _) = t

extractZeta :: Reader ExtractionSpec PT
extractZeta = do
  sig <- extractZetaSig
  rules <- extractZetaRules
  return $ sig rules

extract :: Extraction
extract = do
  epsilon <- extractEpsilon
  zeta <- extractZeta
  return (epsilon, zeta)

getFunIdForRule :: PTRule -> Identifier
getFunIdForRule (PTRule _ pq _) = getFunIdForPQ pq
  where
    getFunIdForPQ (PQApp _ id _) = id
    getFunIdForPQ (PQDes _ _ _ pq) = getFunIdForPQ pq

hasSameIdAsEps :: PTRule -> PT -> Bool
hasSameIdAsEps eps (PTFun _ id _ _ _) = id == (getFunIdForRule eps)
hasSameIdAsEps _ _ = False

instance Eq PP where
  (PPVar _ id) == (PPVar _ id') = id == id'
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

replaceTargetWithEpsilonPT :: PT -> [PTRule] -> PTRule -> PT
replaceTargetWithEpsilonPT pt@(PTFun l id ts t rs) tgt eps = PTFun l id ts t (eps:(rs \\ tgt))

replaceTargetWithEpsilon :: [PT] -> [PTRule] -> PTRule -> [PT]
replaceTargetWithEpsilon pts tgt eps =
  (replaceTargetWithEpsilonPT ((fst partitions)!!0) tgt eps):(snd partitions)
  where
    partitions = partition (hasSameIdAsEps eps) pts

applyExtraction :: ExtractionSpec -> [PT] -> [PT]
applyExtraction spec pts = 
  (replaceTargetWithEpsilon pts (map fst $ target spec) (fst eResult)) ++ [snd eResult]
  where
    eResult = runReader extract spec
