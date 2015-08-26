module UroboroTransformations.Extraction.DesExtraction where

import Uroboro.Tree

import UroboroTransformations.Extraction

desExtractionLensGet :: TQ -> TQ
desExtractionLensGet (TQDes _ _ _ tq) = tq

desExtractionLensPutback :: TQ -> TQ -> TQ
desExtractionLensPutback (TQDes t id tps _) tq = TQDes t id tps tq
desExtractionLensPutback _ tq = tq

desExtractionLens :: ExtractionLens
desExtractionLens = ExtractionLens {get = desExtractionLensGet, putback = desExtractionLensPutback}
