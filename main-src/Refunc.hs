module Main where

import PrettyPrint
import Uroboro.Parser
import Uroboro.Tree
import Uroboro.Error
import Uroboro.Checker

import UroboroTransformations.Refunc

import System.IO
import System.Environment

-- |At the moment this is just a test: 
-- parses, typechecks, refuncs the input, then typechecks and pretty-prints result
-- For this to work, uroboro.cabal needs to be edited to expose Uroboro.PrettyPrint
main :: IO ()
main = do
    input <- getContents
    hPutStrLn stderr "Refunctionalizing... "
    let ptsOrError = parseFile "stdin" input
    case ptsOrError of
        Left e -> hPutStrLn stderr $ unlines ["Parse Error: ", show e]
        Right pts -> case typecheck pts of
            Left e -> hPutStrLn stderr $ unlines ["Typecheck Error: ", show e]
            Right _p -> do
                case refunc pts of
                    Nothing -> hPutStrLn stderr $ "Transform Error: No copattern coverage"
                    Just t -> case typecheck t of
                        Left e -> hPutStrLn stderr $ unlines ["Typecheck Error in transformed program: ", show e]
                        Right _ -> putStrLn $ renderProgram t
    hPutStrLn stderr "Ready."