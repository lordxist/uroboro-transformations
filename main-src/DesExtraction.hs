module Main where

import Control.Monad.Reader

import Uroboro.Parser
import Uroboro.Checker
import Uroboro.Tree
import Uroboro.Error

import PrettyPrint

import UroboroTransformations.Util
import UroboroTransformations.CopatternCoverage.CCTree
import UroboroTransformations.Extraction
import UroboroTransformations.Extraction.DesExtraction

import System.IO
import System.Environment

betterTypecheck :: [PT] -> Either Error Program
betterTypecheck defs = do
    pre  <- foldM preCheckPT emptyProgram defs
    prog <- foldM postCheckPT pre defs
    return prog

desExtractionSpec :: [(PTRule, TQ)] -> ExtractionSpec
desExtractionSpec t = ExtractionSpec {lens = desExtractionLens, target = t}

rulesForFunDef :: PT -> [PTRule]
rulesForFunDef (PTFun _ _ _ _ rs) = rs

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
                    Just tree -> do
                        let tqs = leaves tree
                        let tgt = zipCoverageRules tqs (rulesForFunDef pt)
                        let spec = desExtractionSpec tgt
                        putStrLn $ renderProgram $ applyExtraction spec pts