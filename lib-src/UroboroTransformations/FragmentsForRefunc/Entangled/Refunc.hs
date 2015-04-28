module UroboroTransformations.FragmentsForRefunc.Entangled.Refunc where

import Uroboro.Tree

import qualified UroboroTransformations.CoDataDefsDisj.Refunc as CoDataDefsDisjR

import UroboroTransformations.Util
import UroboroTransformations.Util.HelperFuns

import Data.List(nubBy, groupBy)
import Control.Arrow
import Control.Monad(liftM)
import Control.Monad.State.Lazy
import Control.Monad.Trans.Writer.Lazy

extractWith :: (PQ -> PQ) -> (PQ -> (PP, ([PP], [PP]))) ->[PT] -> PT -> PTRule -> Writer HelperFuns PTRule
extractWith removeLeftCon isolateLeftCon pts (PTFun l id ts t _) r@(PTRule l' pq e) = do
    let helperRule = PTRule l (PQApp l' helperFunName (varsAndLeftCon pq)) e
    let rt = case pq of (PQDes _ des _ _) -> destructorReturnType des pts
                        (PQApp _ _ _)     -> t
    let helperFuns = HelperFuns [PTFun l helperFunName (varsAndLeftConTypes pq) rt [helperRule]]
    tell helperFuns
    return $ PTRule l (removeLeftCon pq) (PApp dummyLocation helperFunName $ map toExpr $ varsReplaceLeftCon pq)
  where
    helperFunName = gensym "extract" (namePattern (removeLeftCon pq)) pts

    varsAndLeftCon pq = recombine $ isolateLeftCon pq
      where
        recombine (c, (vs1, vs2)) = c:(vs1 ++ vs2)

    varsAndLeftConTypes pq = map snd $ (head rZipped1):((reverse $ tail rZipped1) ++ zipped2)
        where
            varTypes = collectVarTypes pts ts $ removeLeftCon pq
            (c, (vs1, vs2)) = isolateLeftCon pq
            zipped1 = zip (vs1++[c]) varTypes
            rZipped1 = reverse zipped1
            zipped2 = zip (reverse vs2) (reverse varTypes)


    varsReplaceLeftCon pq = recombine $ isolateLeftCon pq
      where
        recombine (con, (vs1, vs2)) = flip evalState 0 $ do
            newVs1 <- mapM convertToVar vs1
            v <- convertToVar con
            newVs2 <- mapM convertToVar vs2
            return $ v:(newVs1 ++ newVs2)

extractPatternMatching :: [PT] -> PT -> PTRule -> Writer HelperFuns PTRule
extractPatternMatching pts fun r@(PTRule _ (PQDes _ _ pps pq) _)
    | any con (pps ++ (ppsForPQ pq)) = do
        replacedRule <- extractWith removeLeftCon isolateLeftCon pts fun r
        extractPatternMatching pts fun replacedRule
    | otherwise = return r
  where
    removeLeftCon :: PQ -> PQ
    removeLeftCon (PQApp l id pps) = PQApp l id (evalState (removeLeftConPPs pps) 0)
    removeLeftCon (PQDes l id pps pq)
        | conInPQ pq = PQDes l id pps (removeLeftCon pq)
        | otherwise  = PQDes l id (evalState (removeLeftConPPs pps) 0) pq

    removeLeftConPPs ((v@(PPVar _ _)):pps) = do
        newV <- convertToVar v
        newPPs <- removeLeftConPPs pps
        return $ newV:newPPs
    removeLeftConPPs ((c@(PPCon l id pps)):pps')
        | any con pps = do
            newPPs <- removeLeftConPPs pps
            newPPs' <- mapM renameVars pps'
            return $ (PPCon l id newPPs):newPPs'
        | otherwise = do
            newV <- convertToVar c
            newPPs' <- mapM renameVars pps'
            return $ newV:newPPs'
    removeLeftConPPs [] = return []

    renameVars v@(PPVar _ _) = convertToVar v
    renameVars (PPCon l id pps) = liftM (PPCon l id) $ mapM renameVars pps

    isolateLeftCon :: PQ -> (PP, ([PP], [PP]))
    isolateLeftCon (PQApp _ _ pps) = isolateLeftConPPs pps
    isolateLeftCon (PQDes _ _ pps pq)
        | conInPQ pq = second (second (++(concatMap collectVars pps))) (isolateLeftCon pq)
        | otherwise  = second (first ((collectVarsPQ pq)++)) (isolateLeftConPPs pps)

    isolateLeftConPPs ((v@(PPVar _ _)):pps) = second (first (v:)) (isolateLeftConPPs pps)
    isolateLeftConPPs ((c@(PPCon l id pps)):pps')
        | any con pps = second (second (++(concatMap collectVars pps'))) (isolateLeftConPPs pps)
        | otherwise   = (c, ([], (concatMap collectVars pps')))

    conInPQ (PQDes _ _ pps pq) = (any con pps) || (conInPQ pq)
    conInPQ (PQApp _ _ pps)    = any con pps

    ppsForPQ (PQDes _ _ pps pq) = pps ++ (ppsForPQ pq)
    ppsForPQ (PQApp _ _ pps)    = pps

extractPatternMatching pts fun r@(PTRule _ (PQApp _ _ pps) _)
    | hasTwoCons pps = extractWith removeLeftCon isolateLeftCon pts fun r
    | otherwise = return r
  where
    removeLeftCon = undefined

    isolateLeftCon = undefined

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

    typeToVar _ = convertToVar ()
splitRule _ _ r = [r]

split :: [PT] -> [PT]
split pts = map splitInPT pts
  where
    splitInPT (PTFun l id ts@(t':ts') t rs) = PTFun l id ts t $ concatMap (splitRule pts t') rs
    splitInPT pt = pt

refuncLegal :: [PT] -> Maybe [PT]
refuncLegal pts = CoDataDefsDisjR.refuncLegal $ split $ disentangle pts