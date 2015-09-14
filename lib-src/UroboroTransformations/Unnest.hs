module UroboroTransformations.Unnest where

import UroboroTransformations.CopatternCoverage.CCTree
import UroboroTransformations.Extraction (ExtractionSpec(ExtractionSpec), applyExtraction)
import UroboroTransformations.Extraction.ConExtraction
import UroboroTransformations.Extraction.DesExtraction
import UroboroTransformations.Util
import qualified UroboroTransformations.Util.VariableSchema as Schema

import Uroboro.Tree
import Uroboro.Checker
import Uroboro.Error

import Data.List (deleteBy)
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

unnesting :: (PT, [PT]) -> StateT (CCTree TQ) (State [PT]) [PT]
unnesting (pt, auxs) = do
    tree <- get
    if isSimpleTree tree
      then return (pt:auxs)
      else do
        (pt', aux) <- oneStepUnnesting pt
        modify cutoffLowestSubtree
        lift $ modify (++[aux])
        unnesting (pt', (aux:auxs))

ptToSig :: PT -> PTSig
ptToSig (PTFun l id ts t rs) = (id, (l, ts, t))

unnestingInit :: PT -> ReaderT Program (State [PT]) (Maybe [PT])
unnestingInit pt = do
  prog <- ask
  case runReader (checkCoverage (ptToSig pt) pt) prog of
    Nothing -> return Nothing
    Just c -> lift $ liftM Just $ evalStateT (unnesting (pt, [])) c

programUnnesting :: UnnestPredicate -> [PT] -> Maybe [PT]
programUnnesting i pts = do
    case betterTypecheck pts of
      Left _ -> Nothing
      Right p -> liftM concat $ sequence $ flip evalState pts $ flip runReaderT p $ mapM unnestingInit (filter i pts)

unnestFor :: UnnestPredicate -> [PT] -> Maybe [PT]
unnestFor i pts = case programUnnesting i (map Schema.renameVariables pts) of
  Nothing -> Nothing
  Just pts' -> Just $ pts' ++ (filter (not.i) pts)
