module Main where

import PrettyPrint
import Uroboro.Parser
import Uroboro.Tree
import Uroboro.Error
import Uroboro.Checker

import UroboroTransformations

-- |At the moment this is just a test: 
-- parses, typechecks, defuncs eval1.uro, then typechecks and pretty-prints result
-- For this to work, uroboro.cabal needs to be edited to expose Uroboro.PrettyPrint
main :: IO ()
main = do
    let path = "eval1.uro"
    input <- readFile path
    putStrLn $ "Defunctionalizing " ++ path ++ " ..."
    let ptsOrError = parseFile path input
    case ptsOrError of
        Left e -> putStrLn $ unlines ["Parse Error: ", show e]
        Right pts -> case typecheck pts of
            Left e -> putStrLn $ unlines ["Typecheck Error: ", show e]
            Right _p -> do
                case defunc pts of
                    Nothing -> putStrLn $ "Transform Error: Not in correct Fragment"
                    Just t -> case typecheck t of
                        Left e -> putStrLn $ unlines ["Typecheck Error in transformed program: ", show e]
                        Right _ -> putStrLn $ renderProgram t
    putStrLn "Ready."