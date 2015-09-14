module UroboroTransformations.Util.ForTesting where

import Uroboro.Tree
import Uroboro.Parser(parseFile)

import qualified UroboroTransformations.Util.VariableSchema as Schema

import Data.Either(rights)

import Data.Set (Set, fromList)

standardize :: [PT] -> Set PT
standardize = fromList . (map Schema.renameVariables)

parse :: String -> [PT]
parse s = let ptsOrError = parseFile "here" s in
            ((rights [ptsOrError]) !! 0)