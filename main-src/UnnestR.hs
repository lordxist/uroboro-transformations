module Main where

import Control.Monad.Reader
import Data.List

import Debug.Trace

import Uroboro.Parser
import Uroboro.Checker
import Uroboro.Tree
import Uroboro.Error

import PrettyPrint

import UroboroTransformations.Unnest.ForRefunc

import System.IO
import System.Environment

main :: IO ()
main = do
    input <- getContents
    let ptsOrError = parseFile "stdin" input
    case ptsOrError of
        Left e -> hPutStrLn stderr $ unlines ["Parse Error: ", show e]
        Right pts -> case unnestForRefunc pts of
            Nothing -> hPutStrLn stderr $ "Transform Error: No copattern coverage"
            Just pts' -> putStrLn $ renderProgram pts'