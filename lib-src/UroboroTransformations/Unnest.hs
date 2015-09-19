module UroboroTransformations.Unnest (unnestFor) where

import UroboroTransformations.CopatternCoverage
import UroboroTransformations.CopatternCoverage.CCTree
import UroboroTransformations.Extraction (ExtractionSpec(ExtractionSpec), zipCoverageRules, applyExtraction)
import UroboroTransformations.Extraction.ConExtraction
import UroboroTransformations.Extraction.DesExtraction
import UroboroTransformations.Util
import UroboroTransformations.Util.UroboroEnhanced
import qualified UroboroTransformations.Util.VariableSchema as Schema

import Uroboro.Tree
import Uroboro.Checker
import Uroboro.Error

import Data.Functor.Identity
import Data.List (deleteBy, partition)
import qualified Data.Traversable
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Monad.Trans.Maybe

import Debug.Trace

-- |Describes whether the function definition is not sufficiently unnested for some purpose.
type UnnestPredicate = PT -> Bool

extractionSpec :: CCTree -> [PT] -> [(PTRule, TQ)] -> ExtractionSpec
extractionSpec (ResSplit _ _) = ExtractionSpec desExtractionLens
extractionSpec (VarSplit _ p _) = ExtractionSpec $ conExtractionLens p

oneStepUnnesting :: PT -> ReaderT [PT] (StateT CCTree (State [PT])) (PT, PT)
oneStepUnnesting pt = do
  pts <- ask
  tree <- lift get
  auxs <- lift $ lift get
  let lst = lowestSubtree tree
  let tgt = zipCoverageRules (leaves lst) (rulesForFunDef pt)
  let spec = extractionSpec lst (pts++auxs) tgt
  return $ applyExtraction spec pt

unnestingWithCoverage :: PT -> ReaderT [PT] (StateT CCTree (State [PT])) PT
unnestingWithCoverage pt = do
  tree <- get
  if isSimpleTree tree
    then return pt
    else do
      (pt', aux) <- oneStepUnnesting pt
      lift $ modify cutoffLowestSubtree
      lift $ lift $ modify (++[aux])
      unnestingWithCoverage pt'

ptToSig :: PT -> PTSig
ptToSig (PTFun l id ts t rs) = (id, (l, ts, t))

unnesting :: Program -> [PT] -> PT -> State [PT] (Maybe PT)
unnesting prog pts pt = Data.Traversable.sequence $ do
    c <- runReader (checkCoverage (ptToSig pt) pt) prog
    return $ evalStateT (flip runReaderT pts $ unnestingWithCoverage pt) c

-- |Unnest any Uroboro program with copattern coverage
-- |Fails when the program doesn't have copattern coverage.
unnestFor :: UnnestPredicate -> [PT] -> Maybe [PT]
unnestFor i pts = do
  p <- hush $ betterTypecheck pts
  let (ptsI, ptsNotI) = partition i pts
  let ptsI' = map Schema.renameVariables ptsI
  let u = mapM (unnesting p pts) ptsI'
  ptsI'' <- sequence $ flip evalState [] u
  let auxs = execState u []
  return $ ptsI'' ++ auxs ++ ptsNotI
