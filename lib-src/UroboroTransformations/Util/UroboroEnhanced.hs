module UroboroTransformations.Util.UroboroEnhanced where

import Control.Monad

import Uroboro.Checker
import Uroboro.Tree
import Uroboro.Error

import UroboroTransformations.Util.Typed

data BetterProgram = BetterProgram {
      ts :: [Type]
    , cs :: [(Type, [PTCon])]
    , ds :: [(Type, [PTDes])]
    , fs :: [PTSig]
    , rs :: Rules
}

betterProgram :: Program -> BetterProgram
betterProgram (Program ts cs ds fs rs) = BetterProgram ts (assocWithType cs) (assocWithType ds) fs rs
  where
    assocWithType xs = [(t, filter (belongsToType t) xs) | t <- ts]

type PTSig = (Identifier, (Location, [Type], Type))

betterTypecheck :: [PT] -> Either Error Program
betterTypecheck defs = do
  pre  <- foldM preCheckPT emptyProgram defs
  prog <- foldM postCheckPT pre defs
  return prog
