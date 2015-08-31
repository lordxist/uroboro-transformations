module UroboroTransformations.Extraction.ConExtraction where

import UroboroTransformations.Util (PathToSubterm)
import UroboroTransformations.Extraction

extractionProjectionPP :: PathToSubterm -> PP -> PP
extractionProjectionPP _ (PPVar _ _) = undefined
extractionProjectionPP (p1:p) (PPCon l id pps)
  | p1 == 0 = newvar -- TODO: implement newvar
  | otherwise = PPCon l id (extractionProjectionPPs p pps)

extractionProjectionPPs :: PathToSubterm -> [PP] -> [PP]
extractionProjectionPPs (p1:p) pps =
    ((take p1 pps)++((extractionProjectionPP p) (pps !! p1))++(drop (p1+1) pps))

conExtractionLensGet :: PathToSubterm -> PQ -> PQ
conExtractionLensGet (p1:p) (PQDes l id pq pps) =
  | p1 == 0 = PQDes l id (conExtractionLensGet p pq) pps
  | otherwise = PQDes l id pq (extractionProjectionPPs ((p1-1):p) pps)
conExtractionLensGet (_:p) (PQApp l id pps) = PQApp l id (extractionProjectionPPs p pps)

conExtractionLensPutback :: PathToSubterm -> PQ -> PQ -> PQ
conExtractionLensPutback (PQDes l id pq pps) pq = PQDes l id pq pps

conExtractionLens :: PathToSubterm -> ExtractionLens
conExtractionLens p = ExtractionLens {get = (conExtractionLensGet p), putback = (conExtractionLensPutback p)}
