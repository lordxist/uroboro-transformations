module UroboroTransformations.Util.VariableSchema (nameForPath, renameVariables) where

import Uroboro.Tree

import UroboroTransformations.Util (PathToSubterm)

import Data.Maybe (fromJust)
import Debug.Trace

nameForPath :: PathToSubterm -> String
nameForPath (n:p)
  | null p    = show n
  | otherwise = (show n) ++ "_" ++ (nameForPath p)
nameForPath [] = error "empty path"

varMapPP :: PathToSubterm -> (Int, PP) -> [(Identifier, Identifier)]
varMapPP p (i, (PPVar l id)) = [(id, ("x" ++ (nameForPath (p++[i]))))]
varMapPP p (i, (PPCon l id pps)) = concatMap (varMapPP (p++[i])) (zip [0..] pps)

varMap :: PathToSubterm -> PQ -> [(Identifier, Identifier)]
varMap p (PQApp l id pps) = concatMap (varMapPP p) (zip [0..] pps)
varMap p (PQDes l id pps pq) = (concatMap (varMapPP p) (zip [1..] pps)) ++ (varMap (p++[0]) pq)

renameVarsPExp :: PExp -> [(Identifier, Identifier)] -> PExp
renameVarsPExp (PVar l id) m = PVar l (fromJust $ lookup id m)
renameVarsPExp (PApp l id pexps) m = PApp l id (map (flip renameVarsPExp m) pexps)
renameVarsPExp (PDes l id pexps pexp) m = PDes l id (map (flip renameVarsPExp m) pexps) (renameVarsPExp pexp m)

renameVarsPP :: [(Identifier, Identifier)] -> PP -> PP
renameVarsPP m (PPVar l id) = PPVar l (fromJust $ lookup id m)
renameVarsPP m (PPCon l id pps) = PPCon l id (map (renameVarsPP m) pps)

renameVarsPQ :: PQ -> [(Identifier, Identifier)] -> PQ
renameVarsPQ (PQApp l id pps) m = PQApp l id $ map (renameVarsPP m) pps
renameVarsPQ (PQDes l id pps pq) m = PQDes l id (map (renameVarsPP m) pps) (renameVarsPQ pq m)

renameVariablesInRule :: PTRule -> PTRule
renameVariablesInRule (PTRule l pq pexp) = let vMap = varMap [] pq in
                                             (PTRule l (renameVarsPQ pq vMap) (renameVarsPExp pexp vMap))

renameVariables :: PT -> PT
renameVariables (PTFun l id ts t rs) = PTFun l id ts t $ map renameVariablesInRule rs
renameVariables pt = pt
