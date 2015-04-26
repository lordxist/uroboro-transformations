module Main where

import PrettyPrint
import Uroboro.Parser
import Uroboro.Tree
import Uroboro.Error
import Uroboro.Checker

import UroboroTransformations.Defunc

import System.IO
import System.Environment

-- |At the moment this is just a test: 
-- parses, typechecks, defuncs the input, then typechecks and pretty-prints result
-- For this to work, uroboro.cabal needs to be edited to expose Uroboro.PrettyPrint
main :: IO ()
main = do
    input <- getContents
    hPutStrLn stderr "Defunctionalizing... "
    let ptsOrError = parseFile "stdin" input
    case ptsOrError of
        Left e -> hPutStrLn stderr $ unlines ["Parse Error: ", show e]
        Right pts -> case typecheck pts of
            Left e -> hPutStrLn stderr $ unlines ["Typecheck Error: ", show e]
            Right _p -> do
                case defunc pts of
                    Nothing -> hPutStrLn stderr $ "Transform Error: Not in correct Fragment"
                    Just t -> case typecheck t of
                        Left e -> hPutStrLn stderr $ unlines ["Typecheck Error in transformed program: ", show e]
                        Right _ -> putStrLn $ renderProgram t
    hPutStrLn stderr "Ready."
