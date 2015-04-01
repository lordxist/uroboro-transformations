module Main where

import System.Environment
import PrettyPrint
import Uroboro.Parser
import Uroboro.Tree
import Uroboro.Error
import Uroboro.Checker
import Control.Monad (foldM)

-- | At the moment this is just a test: parses, typechecks and pretty-prints test.uro
-- For this to work, uroboro.cabal needs to be edited to expose Uroboro.PrettyPrint
main :: IO ()
main = do
    let path = "test.uro"
    input <- readFile path
    let ptsOrError = parseFile path input
    case ptsOrError of
        Left e -> putStrLn $ "Parse Error: " ++ (show e)
        Right pts -> case typecheck pts of
            Left e -> putStrLn $ "Typecheck Error: " ++ (show e)
            Right _p -> putStrLn $ renderProgram pts
    putStrLn "Ready."