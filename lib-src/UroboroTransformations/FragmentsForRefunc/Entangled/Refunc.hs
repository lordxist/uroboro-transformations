module UroboroTransformations.FragmentsForRefunc.Entangled.Refunc where

import Uroboro.Tree

import qualified UroboroTransformations.CoDataDefsDisj.Refunc as CoDataDefsDisjR

import UroboroTransformations.Util
import UroboroTransformations.Util.HelperFuns

import Data.List(nubBy, groupBy)
import Control.Monad(liftM)
import Control.Monad.State.Lazy
import Control.Monad.Trans.Writer.Lazy

extractWith :: (PQ -> PQ) -> (PQ -> [PP]) ->[PT] -> PT -> PTRule -> Writer HelperFuns PTRule
extractWith removeLeftCon varsAndLeftCon pts (PTFun l id ts t _) r@(PTRule l' pq e) = do
    let helperRule = PTRule l (PQApp l' helperFunName (varsAndLeftCon pq)) e
    let rt = case pq of (PQDes _ des _ _) -> destructorReturnType des pts
                        (PQApp _ _ _)     -> t
    let helperFuns = HelperFuns [PTFun l helperFunName (collectVarTypes pts ts (removeLeftCon pq)) rt [helperRule]]
    tell helperFuns
    return $ PTRule l (removeLeftCon pq) (PApp dummyLocation helperFunName $ map toExpr $ collectVarsPQ (removeLeftCon pq))
  where
    helperFunName = gensym "extract" (namePattern (removeLeftCon pq)) pts

extractPatternMatching :: [PT] -> PT -> PTRule -> Writer HelperFuns PTRule
extractPatternMatching pts fun r@(PTRule _ (PQDes _ _ pps pq) _)
    | any con (pps ++ (ppsForPQ pq)) = extractWith removeLeftCon varsAndLeftCon pts fun r
    | otherwise = return r
  where
    removeLeftCon (PQApp l id pps) = PQApp l id (removeLeftConPPs pps)
    removeLeftCon (PQDes l id pps pq)
        | conInPQ pq = PQDes l id pps (removeLeftCon pq)
        | otherwise  = PQDes l id (removeLeftConPPs pps) pq

    varsAndLeftCon (PQApp _ _ pps) = varsAndLeftConPPs pps
    varsAndLeftCon (PQDes _ _ pps pq)
        | conInPQ pq = (varsAndLeftCon pq) ++ (concatMap collectVars pps)
        | otherwise  = (collectVarsPQ pq) ++ (varsAndLeftConPPs pps)

    varsAndLeftConPPs ((v@(PPVar _ _)):pps) = v:(varsAndLeftConPPs pps)
    varsAndLeftConPPs ((c@(PPCon l id pps)):pps')
        | any con pps = (varsAndLeftConPPs pps) ++ (concatMap collectVars pps')
        | otherwise   = c:(concatMap collectVars pps')
    varsAndLeftConPPs [] = []

    conInPQ (PQDes _ _ pps pq) = (any con pps) || (conInPQ pq)
    conInPQ (PQApp _ _ pps)    = any con pps

    removeLeftConPPs ((v@(PPVar _ _)):pps) = v:(removeLeftConPPs pps)
    removeLeftConPPs ((c@(PPCon l id pps)):pps')
        | any con pps = (PPCon l id (removeLeftConPPs pps)):pps'
        | otherwise   = (PPVar l id):pps' -- TODO: after this, all variable names need to be renamed
    removeLeftConPPs [] = []

    ppsForPQ (PQDes _ _ pps pq) = pps ++ (ppsForPQ pq)
    ppsForPQ (PQApp _ _ pps)    = pps

extractPatternMatching pts fun r@(PTRule _ (PQApp _ _ pps) _)
    | hasTwoCons pps = extractWith removeLeftCon varsAndLeftCon pts fun r
    | otherwise = return r
  where
    removeLeftCon = undefined

    varsAndLeftCon = undefined

    hasTwoCons = undefined

disentangle :: [PT] -> [PT]
disentangle = extractHelperFuns extractPatternMatching

splitRule :: [PT] -> Type -> PTRule -> [PTRule]
splitRule pts t (PTRule l (PQApp l' id ((PPVar _ id'):pps)) e) =
    map makeRuleForCon $ consForType pts
  where
    consForType ((PTPos _ _ cons@((PTCon _ t' _ _):_)):pts')
        | t == t' = cons
        | otherwise          = consForType pts'
    consForType (_:pts') = consForType pts'

    makeRuleForCon c = (PTRule l (PQApp l' id (convertPPs c)) e)

    convertPPs (PTCon _ _ cId ts) = flip evalState 0 $ do
        varsForCon <- mapM typeToVar ts
        let ppCon = PPCon l cId varsForCon
        otherVars <- mapM convertToVar pps
        return $ [ppCon] ++ otherVars

    typeToVar _ = convertToVar (PPVar dummyLocation "")
splitRule _ _ r = [r]

convertToVar :: PP -> State Int PP
convertToVar _ = do
    n <- get
    modify (+1)
    return $ PPVar dummyLocation ("x"++(show n))

split :: [PT] -> [PT]
split pts = map splitInPT pts
  where
    splitInPT (PTFun l id ts@(t':ts') t rs) = PTFun l id ts t $ concatMap (splitRule pts t') rs
    splitInPT pt = pt

refuncLegal :: [PT] -> Maybe [PT]
refuncLegal pts = CoDataDefsDisjR.refunc $ split $ disentangle pts