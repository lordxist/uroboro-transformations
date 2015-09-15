module UroboroTransformations.Extraction.ConExtraction (conExtractionLens) where

import Control.Monad
import Control.Monad.Reader

import Uroboro.Tree

import UroboroTransformations.Util (PathToSubterm, largestVarIndex, tqVarIds, containsVar, containsVarTP, newvarIndex)
import UroboroTransformations.Util.VariableSchema
import UroboroTransformations.Extraction

extractionProjectionTP :: PathToSubterm -> TP -> Reader PathToSubterm TP
extractionProjectionTP _ (TPVar _ _) = undefined
extractionProjectionTP [] (TPCon t id tps) = reader $ (TPVar t).("x"++).nameForPath
extractionProjectionTP p@(_:_) (TPCon t id tps) = (liftM $ TPCon t id) (extractionProjectionTPs p tps)

extractionProjectionTPs :: PathToSubterm -> [TP] -> Reader PathToSubterm [TP]
extractionProjectionTPs (p1:p) tps = do
    tp <- extractionProjectionTP p (tps !! p1)
    return $ ((take p1 tps)++[tp]++(drop (p1+1) tps))

conExtractionLensGetReader :: PathToSubterm -> TQ -> Reader PathToSubterm TQ
conExtractionLensGetReader (p1:p) (TQDes t id tps tq)
  | p1 == 0 = (liftM $ TQDes t id tps) (conExtractionLensGetReader p tq)
  | otherwise = (liftM $ (flip $ TQDes t id) tq) (extractionProjectionTPs ((p1-1):p) tps)
conExtractionLensGetReader p (TQApp t id tps) = (liftM $ TQApp t id) (extractionProjectionTPs p tps)

conExtractionLensGet :: PathToSubterm -> TQ -> TQ
conExtractionLensGet p tq = runReader (conExtractionLensGetReader p tq) p

walkToConstructorTP :: PathToSubterm -> TP -> TP
walkToConstructorTP (n:p) (TPCon t id tps) = walkToConstructorTP p (tps !! n)
walkToConstructorTP [] tpcon@(TPCon _ _ _) = tpcon

walkToConstructor :: PathToSubterm -> TQ -> TP
walkToConstructor (0:p) (TQDes t id tps tq) = walkToConstructor p tq
walkToConstructor (n:p) (TQDes t id tps tq) = walkToConstructorTP p (tps !! (n-1))
walkToConstructor (n:p) (TQApp t id tps) = walkToConstructorTP p (tps !! n)

putbackTP :: PathToSubterm -> TQ -> TP -> TP
putbackTP p tq tpcon@(TPCon t id tps)
  | any (containsVarTP ("x"++(nameForPath p))) tps = TPCon t id (map (putbackTP p tq) tps)
  | otherwise = tpcon
putbackTP p tq tpvar@(TPVar t id)
  | id == ("x"++(nameForPath p)) = walkToConstructor p tq
  | otherwise = tpvar

conExtractionLensPutback :: PathToSubterm -> TQ -> TQ -> TQ
conExtractionLensPutback p tq (TQDes t id tps tq')
  | containsVar tq' ("x"++(nameForPath p)) = TQDes t id tps (conExtractionLensPutback p tq tq')
  | otherwise = TQDes t id (map (putbackTP p tq) tps) tq'
conExtractionLensPutback p tq (TQApp t id tps) = TQApp t id (map (putbackTP p tq) tps)

conExtractionLens :: PathToSubterm -> ExtractionLens
conExtractionLens p = ExtractionLens {get = (conExtractionLensGet p), putback = (conExtractionLensPutback p)}
