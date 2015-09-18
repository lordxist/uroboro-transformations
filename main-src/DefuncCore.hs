module Main where

import PrettyPrint
import Uroboro.Parser
import Uroboro.Tree
import Uroboro.Error
import Uroboro.Checker

import UroboroTransformations.CoreDR.CoreDefunc

import System.IO
import System.Environment

-- |Applies core refunc. to the input
main :: IO ()
main = do
    input <- getContents
    hPutStrLn stderr "Defunctionalizing (core)... "
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