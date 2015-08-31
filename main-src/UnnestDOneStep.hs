module Main where

import Control.Monad.Reader
import Data.List
import Data.Maybe

import Uroboro.Parser
import Uroboro.Checker
import Uroboro.Tree
import Uroboro.Error

import PrettyPrint

import UroboroTransformations.Util
import UroboroTransformations.CopatternCoverage.CCTree
import UroboroTransformations.Extraction
import UroboroTransformations.Extraction.DesExtraction
import UroboroTransformations.Extraction.ConExtraction

import System.IO
import System.Environment

betterTypecheck :: [PT] -> Either Error Program
betterTypecheck defs = do
    pre  <- foldM preCheckPT emptyProgram defs
    prog <- foldM postCheckPT pre defs
    return prog

extractionSpec :: CCTree TQ -> [PT] -> [(PTRule, TQ)] -> ExtractionSpec
extractionSpec (ResSplit _ _) = ExtractionSpec desExtractionLens
extractionSpec (VarSplit _ p _) = ExtractionSpec $ conExtractionLens p

rulesForFunDef :: PT -> [PTRule]
rulesForFunDef (PTFun _ _ _ _ rs) = rs

isDNestedFunDef :: PT -> Bool
isDNestedFunDef (PTFun _ _ _ _ rs) = any isDNestedRule rs
  where
    isDNestedRule (PTRule _ (PQDes _ _ _ (PQDes _ _ _ _)) _) = True
    isDNestedRule _ = False
isDNestedFunDef _ = False

ptToSig :: PT -> PTSig
ptToSig (PTFun l id ts t rs) = (id, (l, ts, t))

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
                let ptOrNothing = find isDNestedFunDef pts
                case ptOrNothing of
                    Nothing -> do
                        hPutStrLn stderr "Note: Program is already unnested for defunc."
                        putStrLn $ renderProgram $ pts
                    Just pt -> do
                        let sig = ptToSig pt
                        let coverage = runReader (checkCoverage sig pt) p
                        case coverage of
                            Nothing -> hPutStrLn stderr $ "No copattern coverage"
                            Just tree -> do
                                let lst = lowestSubtree tree
                                let tgt = zipCoverageRules (leaves lst) (rulesForFunDef pt)
                                let spec = extractionSpec lst pts tgt
                                putStrLn $ renderProgram $ applyExtraction spec pts