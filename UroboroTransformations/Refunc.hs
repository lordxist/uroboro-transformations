module UroboroTransformations.Refunc where

import Uroboro.Tree

import qualified UroboroTransformations.FragmentsForRefunc.Entangled.Refunc as EntangledR

elimDesFromMixeds :: [PT] -> [PT]
elimDesFromMixeds = id -- TODO: implement

-- |Refunctionalize an Uroboro program
refunc :: [PT] -> Maybe [PT]
refunc pts = EntangledR.refuncLegal $ elimDesFromMixeds pts