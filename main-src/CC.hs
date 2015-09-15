module Main where

import Control.Monad.Reader

import Uroboro.Parser
import Uroboro.Checker
import Uroboro.Tree
import Uroboro.Error

import UroboroTransformations.Util
import UroboroTransformations.CopatternCoverage
import UroboroTransformations.CopatternCoverage.CCTree

import System.IO
import System.Environment

main :: IO ()
main = do
    input <- getContents
    hPutStrLn stderr "Checking Copattern Coverage... "
    let ptsOrError = parseFile "stdin" input
    case ptsOrError of
        Left e -> hPutStrLn stderr $ unlines ["Parse Error: ", show e]
        Right pts -> case betterTypecheck pts of
            Left e -> hPutStrLn stderr $ unlines ["Typecheck Error: ", show e]
            Right p -> do
                let sig = (fs $ betterProgram p) !! 0
                let pt = pts !! 1
                let coverage = runReader (checkCoverage sig pt) p
                case coverage of
                    Nothing -> hPutStrLn stderr $ "No copattern coverage"
                    Just tree -> putStrLn $ show $ tree