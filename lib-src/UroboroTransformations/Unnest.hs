module UroboroTransformations.Unnest (unnestFor) where

import UroboroTransformations.CopatternCoverage
import UroboroTransformations.CopatternCoverage.CCTree
import UroboroTransformations.Extraction (ExtractionSpec(ExtractionSpec), applyExtraction)
import UroboroTransformations.Extraction.ConExtraction
import UroboroTransformations.Extraction.DesExtraction
import UroboroTransformations.Util
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

type UnnestPredicate = PT -> Bool

extractionSpec :: CCTree TQ -> [PT] -> [(PTRule, TQ)] -> ExtractionSpec
extractionSpec (ResSplit _ _) = ExtractionSpec desExtractionLens
extractionSpec (VarSplit _ p _) = ExtractionSpec $ conExtractionLens p

oneStepUnnesting :: PT -> StateT (CCTree TQ) (State [PT]) (PT, PT) 
oneStepUnnesting pt = do
  tree <- get
  pts <- lift get
  let lst = lowestSubtree tree
  let tgt = zipCoverageRules (leaves lst) (rulesForFunDef pt)
  let spec = extractionSpec lst pts tgt
  return $ applyExtraction spec pt

unnestingWithCoverage :: (PT, [PT]) -> StateT (CCTree TQ) (State [PT]) [PT]
unnestingWithCoverage (pt, auxs) = do
    tree <- get
    if isSimpleTree tree
      then return (pt:auxs)
      else do
        (pt', aux) <- oneStepUnnesting pt
        modify cutoffLowestSubtree
        lift $ modify (++[aux])
        unnestingWithCoverage (pt', (aux:auxs))

ptToSig :: PT -> PTSig
ptToSig (PTFun l id ts t rs) = (id, (l, ts, t))

unnesting :: Program -> PT -> State [PT] (Maybe [PT])
unnesting prog pt = Data.Traversable.sequence $ do
  c <- runReader (checkCoverage (ptToSig pt) pt) prog
  return $ evalStateT (unnestingWithCoverage (pt, [])) c

unnestFor :: UnnestPredicate -> [PT] -> Maybe [PT]
unnestFor i pts = do
  p <- hush $ betterTypecheck pts
  let (ptsI, ptsNotI) = partition i pts
  let ptsI' = map Schema.renameVariables ptsI
  ptsI'' <- liftM concat $ sequence $ flip evalState pts $ mapM (unnesting p) ptsI'
  return $ ptsI'' ++ ptsNotI
