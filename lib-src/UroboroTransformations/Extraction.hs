module UroboroTransformations.Extraction (
    ExtractionLens (ExtractionLens, get, putback)
  , ExtractionSpec (ExtractionSpec, lens, fullProg, target)
  , applyExtraction
) where

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
import UroboroTransformations.Util.Typed
import UroboroTransformations.Util.Conversion

data ExtractionLens = ExtractionLens {
      get     :: TQ -> TQ
    , putback :: TQ -> TQ -> TQ
}

data ExtractionSpec = ExtractionSpec {
      lens     :: ExtractionLens
    , fullProg :: [PT]
    , target   :: [(PTRule, TQ)]
}

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

applyExtraction :: ExtractionSpec -> PT -> (PT, PT)
applyExtraction spec pt = first (replaceTargetWithEpsilon pt (map fst $ target spec)) eResult
  where
    eResult = runReader extract spec
