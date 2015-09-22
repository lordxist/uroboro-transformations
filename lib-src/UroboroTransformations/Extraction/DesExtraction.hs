module UroboroTransformations.Extraction.DesExtraction (desExtractionLens) where

import Uroboro.Tree

import UroboroTransformations.Extraction

desExtractionLensGet :: TQ -> TQ
desExtractionLensGet (TQDes _ _ _ tq) = tq

desExtractionLensPutback :: TQ -> TQ -> TQ
desExtractionLensPutback (TQDes t id tps _) tq = TQDes t id tps tq
desExtractionLensPutback _ tq = tq

-- |Extraction lens for destructor extraction
desExtractionLens :: ExtractionLens
desExtractionLens = ExtractionLens {get = desExtractionLensGet, putback = desExtractionLensPutback}
