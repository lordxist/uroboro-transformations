module UroboroTransformations.Unnest where

import UroboroTransformations.CopatternCoverage.CCTree
import UroboroTransformations.Extraction (ExtractionSpec(ExtractionSpec), applyExtraction)
import UroboroTransformations.Extraction.ConExtraction
import UroboroTransformations.Extraction.DesExtraction
import UroboroTransformations.Util

import Uroboro.Tree
import Uroboro.Checker
import Uroboro.Error

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

oneStepUnnesting :: PT -> StateT (CCTree TQ) (ReaderT [PT] (Writer [PT])) PT
oneStepUnnesting pt = do
  tree <- get
  pts <- lift ask
  let lst = lowestSubtree tree
  let tgt = zipCoverageRules (leaves lst) (rulesForFunDef pt)
  let spec = extractionSpec lst pts tgt
  lift $ lift $ applyExtraction spec pt

unnesting :: PT -> StateT (CCTree TQ) (ReaderT [PT] (Writer [PT])) PT
unnesting pt = do
  tree <- get
  if isSimpleTree tree
    then return pt
    else do
      pt' <- oneStepUnnesting pt
      modify cutoffLowestSubtree
      unnesting pt'

ptToSig :: PT -> PTSig
ptToSig (PTFun l id ts t rs) = (id, (l, ts, t))

unnestingInit :: PT -> ReaderT Program (MaybeT (ReaderT [PT] (Writer [PT]))) PT
unnestingInit pt = do
  prog <- ask
  case runReader (checkCoverage (ptToSig pt) pt) prog of
    Nothing -> lift $ (MaybeT . return) Nothing
    Just c -> lift $ lift $ evalStateT (unnesting pt) c

programUnnesting :: UnnestPredicate -> [PT] -> Writer [PT] (Maybe [PT])
programUnnesting i pts = do
    case betterTypecheck pts of
      Left _ -> return Nothing
      Right p -> liftM sequence $ mapM ((flip runReaderT pts).runMaybeT.(flip runReaderT p).unnestingInit) (filter i pts)

unnestFor :: UnnestPredicate -> [PT] -> Maybe [PT]
unnestFor i pts = case runWriter (programUnnesting i pts) of
  (Nothing, _) -> Nothing
  (Just pts', aux) -> Just $ pts' ++ (filter (not.i) pts) ++ aux
