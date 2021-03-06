module UroboroTransformations.CoreDR.MoveCon (moveConFront) where

import qualified UroboroTransformations.Extraction as Extr
import UroboroTransformations.Extraction.ConExtraction
import UroboroTransformations.Util
import UroboroTransformations.Util.UroboroEnhanced

import Uroboro.Checker
import Uroboro.Tree

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Data.Maybe

moveConTarget :: PT -> Program -> [(PTRule, TQ)]
moveConTarget (PTFun _ id _ _ rs) prog = zip rs (map fst (fromJust $ lookup id (rules prog)))

pathToCon :: PQ -> Int
pathToCon (PQApp _ _ pps) = argNumOfCon pps
  where
    argNumOfCon ((PPCon _ _ _):pps) = 0
    argNumOfCon (_:pps) = 1 + (argNumOfCon pps)

spec :: PT -> State ([PT], Program) (Extr.ExtractionSpec, Int)
spec pt@(PTFun _ _ _ _ ((PTRule _ pq _):_)) = do
    (pts, prog) <- get
    let n = pathToCon pq
    return ((Extr.ExtractionSpec (conExtractionLens [n]) pts (moveConTarget pt prog)), n)

extractCon :: PT -> State ([PT], Program) ((PT, PT), Int)
extractCon pt = do
    (sp, n) <- spec pt
    let (pt', aux) = Extr.applyExtraction sp pt
    modify (first (++[aux]))
    return ((pt', aux), n)

isAffected :: PT -> Bool
isAffected (PTFun _ _ _ _ ((PTRule _ (PQApp _ _ (_:pps)) _):_)) = any con pps
isAffected _ = False

moveToFrontInPQ :: Int -> PQ -> PQ
moveToFrontInPQ n (PQApp l id pps) = PQApp l id ((pps !! n):((take n pps)++(drop (n+1) pps)))

moveToFrontInAux :: Int -> PT -> PT
moveToFrontInAux n (PTFun l id ts t rs) = PTFun l id moveInTs t (map moveInRule rs)
  where
    moveInTs = (ts !! n):((take n ts)++(drop (n+1) ts))
    moveInRule (PTRule l pq pexp) = PTRule l (moveToFrontInPQ n pq) pexp

moveToFrontInCallSitesPExp :: [(Identifier, Int)] -> PExp -> PExp
moveToFrontInCallSitesPExp ids (PApp l id pexps)
  | id `elem` (map fst ids) = let n = fromJust $ lookup id ids in
                                let pexps' = map (moveToFrontInCallSitesPExp ids) pexps in
                                  PApp l id ((pexps' !! n):((take n pexps')++(drop (n+1) pexps')))
  | otherwise = PApp l id (map (moveToFrontInCallSitesPExp ids) pexps)
moveToFrontInCallSitesPExp ids (PDes l id pexps pexp) =
    PDes l id (map (moveToFrontInCallSitesPExp ids) pexps) (moveToFrontInCallSitesPExp ids pexp)
moveToFrontInCallSitesPExp ids pvar = pvar

moveToFrontInCallSitesPTRule :: [(Identifier, Int)] -> PTRule -> PTRule
moveToFrontInCallSitesPTRule ids (PTRule l pq pexp) = PTRule l pq $ moveToFrontInCallSitesPExp ids pexp

moveToFrontInCallSites :: [(Identifier, Int)] -> PT -> PT
moveToFrontInCallSites ids (PTFun l id ts t rs) = PTFun l id ts t (map (moveToFrontInCallSitesPTRule ids) rs)
moveToFrontInCallSites _ pt = pt

movingConFront :: State ([PT], Program) [PT]
movingConFront = do
    (pts, _) <- get
    ptsWithAuxs <- mapM extractCon (filter isAffected pts)
    let (pts', auxs) = unzip (map fst ptsWithAuxs)
    let ns = map snd ptsWithAuxs
    let auxs' = map (uncurry moveToFrontInAux) $ zip ns auxs
    let otherPts = filter (not.isAffected) pts
    return $ map (moveToFrontInCallSites $ zip (map ptFunId auxs) ns) $ pts' ++ auxs' ++ otherPts
  where
    ptFunId (PTFun _ id _ _ _) = id

-- | Move all constructors to the front, by extracting them and switching the arguments
-- in the auxiliary function definition. Fails when the program doesn't typecheck.
moveConFront :: [PT] -> Maybe [PT]
moveConFront pts = case betterTypecheck pts of
                     Left _ -> Nothing
                     Right prog -> Just $ evalState movingConFront (pts, prog)

