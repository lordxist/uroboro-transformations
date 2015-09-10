module UroboroTransformations.Extraction where

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Data.List
import Debug.Trace

import Uroboro.Parser
import Uroboro.Checker
import Uroboro.Tree
import Uroboro.Error

import UroboroTransformations.Util

data ExtractionLens = ExtractionLens {
      get     :: TQ -> TQ
    , putback :: TQ -> TQ -> TQ
}

data ExtractionSpec = ExtractionSpec {
      lens     :: ExtractionLens
    , fullProg :: [PT]
    , target   :: [(PTRule, TQ)]
}

type Extraction = Reader ExtractionSpec (PTRule, PT)

typeOfTQ :: TQ -> Type
typeOfTQ (TQApp t _ _) = t
typeOfTQ (TQDes t _ _ _) = t

auxName :: Reader ExtractionSpec Identifier
auxName = do
  ExtractionSpec _ prog _ <- ask
  return $ autogen "aux" prog

auxify :: TQ -> Reader ExtractionSpec TExp
auxify tq = do
    id <- auxName
    return $ TApp (typeOfTQ tq) id (map tpToTExp (collectVarsTQ tq))
  where
    tpToTExp (TPVar t id) = TVar t id

epsilonLhs :: Reader ExtractionSpec TQ
epsilonLhs = do
    (ExtractionSpec lens _ target) <- ask
    let (_, tq) = target !! 0
    return $ (get lens) tq

epsilonRhs :: Reader ExtractionSpec TExp
epsilonRhs = epsilonLhs >>=  auxify

tpToPP :: TP -> PP
tpToPP (TPVar t id) = PPVar dummyLocation id
tpToPP (TPCon t id tps) = PPCon dummyLocation id (map tpToPP tps)

tqToPQ :: TQ -> PQ
tqToPQ (TQApp t id tps) = PQApp dummyLocation id (map tpToPP tps)
tqToPQ (TQDes t id tps tq) = PQDes dummyLocation id (map tpToPP tps) (tqToPQ tq)    

extractEpsilon :: Reader ExtractionSpec PTRule
extractEpsilon = do
    ExtractionSpec _ _ (((PTRule l _ _), _):_) <- ask
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
    (ExtractionSpec lens _ target) <- ask
    lhs <- epsilonRhs
    return $ map (replacePQ . (second (tqToPQ . ((flip $ putback lens) (tExpToTQ lhs))))) target
  where
    replacePQ ((PTRule l  _ pexp), pq) = PTRule l pq pexp

    tExpToTQ (TApp t id tExps) = TQApp t id (map tExpToTP tExps)
    tExpToTQ (TDes t id tExps tExp) = TQDes t id (map tExpToTP tExps) (tExpToTQ tExp)

    tExpToTP (TVar t id) = TPVar t id
    tExpToTP (TCon t id tExps) = TPCon t id (map tExpToTP tExps)

extractZetaSig :: Reader ExtractionSpec ([PTRule] -> PT)
extractZetaSig = do
    texp <- epsilonRhs
    id <- auxName
    return $ PTFun dummyLocation id (argTs texp) (resT texp)
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

replaceTargetWithEpsilon :: PT -> [PTRule] -> PTRule -> PT
replaceTargetWithEpsilon pt@(PTFun l id ts t rs) tgt eps = PTFun l id ts t (eps:(rs \\ tgt))

applyExtraction :: ExtractionSpec -> PT -> (PT, PT)
applyExtraction spec pt = first (replaceTargetWithEpsilon pt (map fst $ target spec)) eResult
  where
    eResult = runReader extract spec
