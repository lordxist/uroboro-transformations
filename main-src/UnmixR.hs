module Main where

import PrettyPrint
import Uroboro.Parser
import Uroboro.Tree
import Uroboro.Error
import Uroboro.Checker

-- TODO: implement this module
--import UroboroTransformations.FragmentsForRefunc.MixedDefs.Refunc

import System.IO
import System.Environment

-- |Not implemented yet, later it:
-- parses, typechecks, unmixes the input for refunc., then typechecks and pretty-prints result
-- For this to work, uroboro.cabal needs to be edited to expose Uroboro.PrettyPrint
main :: IO ()
main = do
    input <- getContents
    hPutStrLn stderr "Unmixing (refunc.)... "
    let ptsOrError = parseFile "stdin" input
    case ptsOrError of
        Left e -> hPutStrLn stderr $ unlines ["Parse Error: ", show e]
        Right pts -> case typecheck pts of
            Left e -> hPutStrLn stderr $ unlines ["Typecheck Error: ", show e]
            Right _p -> do
                let t = pts -- TODO: implement module above and use it here
                case typecheck t of
                    Left e -> hPutStrLn stderr $ unlines ["Typecheck Error in transformed program: ", show e]
                    Right _ -> putStrLn $ renderProgram t
    hPutStrLn stderr "Ready."