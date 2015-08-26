module UroboroTransformations.Extraction.ConExtraction where

import UroboroTransformations.Extraction

-- problem: need a TQ with location information (see data type ExtractionLens)

{-extractionProjectionPP :: Location -> PP -> PP
extractionProjectionPP l var@(PPVar _ _) = var
extractionProjectionPP l (PPCon l' id pps)
  | l == l' = newvar
  | otherwise = PPCon l' id (map (extractionProjectionPP l) pps)

conExtractionLensGet :: Location -> PQ -> PQ
conExtractionLensGet l (PQDes l' id pq pps) =
    PQDes l' id (conExtractionLensGet l pq) (map (extractionProjectionPP l) pps)
conExtractionLensGet l (PQApp l' id pps) = PQApp l' id (map (extractionProjectionPP l) pps)

conExtractionLensPutback :: Location -> PQ -> PQ -> PQ
conExtractionLensPutback (PQDes l id pq pps) pq = PQDes l id pq pps

conExtractionLens :: ExtractionLens
conExtractionLens = ExtractionLens {get = conExtractionLensGet, putback = conExtractionLensPutback}
-}