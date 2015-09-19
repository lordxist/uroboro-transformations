module UroboroTransformations.Extraction (
    ExtractionLens (ExtractionLens, get, putback)
  , ExtractionSpec (ExtractionSpec, lens, fullProg, target)
  , zipCoverageRules
  , applyExtraction
) where

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Data.List
import Data.Maybe
import Debug.Trace

import Uroboro.Parser
import Uroboro.Checker
import Uroboro.Tree
import Uroboro.Error

import UroboroTransformations.Util
import UroboroTransformations.Util.Typed
import UroboroTransformations.Util.Conversion

-- The lens underlying the extraction.
data ExtractionLens = ExtractionLens {
      get     :: TQ -> TQ
    , putback :: TQ -> TQ -> TQ
}

-- |Describes an extraction.
data ExtractionSpec = ExtractionSpec {
      lens     :: ExtractionLens -- the underlying lens
    , fullProg :: [PT] -- the whole program (needed for autogenerating function names)
    , target   :: [(PTRule, TQ)] -- the extraction target
}

fitVariablesTP :: TP -> PP -> TP
fitVariablesTP (TPVar t _) (PPVar _ id) = TPVar t id
fitVariablesTP (TPCon t id tps) (PPCon _ _ pps) = TPCon t id (map (uncurry fitVariablesTP) (zip tps pps))

fitVariablesTQ :: TQ -> PQ -> TQ
fitVariablesTQ (TQDes t id tps tq) (PQDes _ _ pps pq) =
  TQDes t id (map (uncurry fitVariablesTP) (zip tps pps)) (fitVariablesTQ tq pq)
fitVariablesTQ (TQApp t id tps) (PQApp _ _ pps) =
  TQApp t id (map (uncurry fitVariablesTP) (zip tps pps))

-- |Utility to zip rules with typed lhss.
zipCoverageRules :: [TQ] -> [PTRule] -> [(PTRule, TQ)]
zipCoverageRules [] _ = []
zipCoverageRules (tq:tqs) rs
    = (fitVariables ((fromJust $ find fitsWithTQ rs),tq)):(zipCoverageRules tqs rs)
  where
    fitsWithTQ (PTRule _ pq _) = (tqToPQ tq) == pq

    fitVariables (r@(PTRule _ pq _), tq) = (r, (fitVariablesTQ tq pq))

auxName :: Reader ExtractionSpec Identifier
auxName = do
  ExtractionSpec _ prog _ <- ask
  return $ autogen "aux" prog

auxify :: TQ -> Reader ExtractionSpec TExp
auxify tq = liftM (flip (TApp (getType tq)) (map tpToTExp (collectVarsTQ tq))) auxName    

epsilonLhs :: Reader ExtractionSpec TQ
epsilonLhs = do
    (ExtractionSpec lens _ target) <- ask
    let (_, tq) = target !! 0
    return $ (get lens) tq

epsilonRhs :: Reader ExtractionSpec TExp
epsilonRhs = epsilonLhs >>= auxify   

extractEpsilon :: Reader ExtractionSpec PTRule
extractEpsilon = do
    ExtractionSpec _ _ (((PTRule l _ _), _):_) <- ask
    lhs <- epsilonLhs
    rhs <- epsilonRhs
    return $ PTRule l (tqToPQ lhs) (tExpToPExp rhs)

extractZetaRules :: Reader ExtractionSpec [PTRule]
extractZetaRules = do
    (ExtractionSpec lens _ target) <- ask
    lhs <- epsilonRhs
    return $ map (replacePQ . (second (tqToPQ . ((flip $ putback lens) (tExpToTQ lhs))))) target
  where
    replacePQ ((PTRule l  _ pexp), pq) = PTRule l pq pexp

extractZetaSig :: Reader ExtractionSpec ([PTRule] -> PT)
extractZetaSig = do
    texp <- epsilonRhs
    id <- auxName
    return $ PTFun dummyLocation id (argTs texp) (getType texp)
  where
    argTs (TApp _ _ texps) = map getType texps

extractZeta :: Reader ExtractionSpec PT
extractZeta = do
  sig <- extractZetaSig
  rules <- extractZetaRules
  return $ sig rules

extract :: Reader ExtractionSpec (PTRule, PT)
extract = do
  epsilon <- extractEpsilon
  zeta <- extractZeta
  return (epsilon, zeta)

replaceTargetWithEpsilon :: PT -> [PTRule] -> PTRule -> PT
replaceTargetWithEpsilon (PTFun l id ts t rs) tgt eps = PTFun l id ts t (eps:(rs \\ tgt))

-- |Apply the extraction as represented by the given ExtractionSpec to the given function definition.
-- Returns a pair of function definitions, the first is the changed original definition, the other
-- is the generated auxiliary definition.
applyExtraction :: ExtractionSpec -> PT -> (PT, PT)
applyExtraction spec pt = first (replaceTargetWithEpsilon pt (map fst $ target spec)) eResult
  where
    eResult = runReader extract spec
