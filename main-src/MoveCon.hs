module Main where

import Uroboro.Parser
import Uroboro.Checker
import Uroboro.Tree
import Uroboro.Error

import UroboroTransformations.MoveCon

import PrettyPrint

import System.IO
import System.Environment

main :: IO ()
main = do
    input <- getContents
    let ptsOrError = parseFile "stdin" input
    case ptsOrError of
        Left e -> hPutStrLn stderr $ unlines ["Parse Error: ", show e]
        Right pts -> case moveConFront pts of
            Nothing -> hPutStrLn stderr "Error during moving"
            Just pts' -> putStrLn $ renderProgram $ pts'