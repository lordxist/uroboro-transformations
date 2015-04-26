module PrettyPrint where
  
import Uroboro.PrettyPrint hiding (args, render)
import Uroboro.Checker
import Uroboro.Tree

args :: [a] -> (a -> String) -> String
args as f = parens (commaSep (map f as))

renderType :: Type -> String
renderType (Type t) = t

renderExp :: PExp -> String
renderExp (PVar _ id) = id
renderExp (PApp _ id es) = id ++ (args es renderExp)
renderExp (PDes _ id es e) = (renderExp e) ++ "." ++ id ++ (args es renderExp)

renderCon :: PTCon -> String
renderCon (PTCon _ t id ts) = id ++ (args ts renderType) ++ ": " ++ (renderType t)

renderDes :: PTDes -> String
renderDes (PTDes _ t id ts t') = 
  (renderType t') ++ "." ++ id ++ (args ts renderType) ++ ": " ++ (renderType t)

renderPP :: PP -> String
renderPP (PPVar _ id) = id
renderPP (PPCon _ id pps) = id ++ (args pps renderPP)

renderPQ :: PQ -> String
renderPQ (PQApp _ id pps) = id ++ (args pps renderPP)
renderPQ (PQDes _ id pps pq) = (renderPQ pq) ++ "." ++ id ++ (args pps renderPP) 

renderRule :: PTRule -> String
renderRule (PTRule _ pq e) = (renderPQ pq) ++ " = " ++ (renderExp e)

renderSignature :: PT -> String
renderSignature (PTPos _ t _) = "data " ++ (renderType t)
renderSignature (PTNeg _ t _) = "codata " ++ (renderType t)
renderSignature (PTFun _ id ts t _) = 
  "function " ++ id ++ (args ts renderType) ++ ": " ++ (renderType t)

indent :: String -> String
indent s = "  " ++ s

renderDefs :: PT -> String
renderDefs (PTPos _ _ cons)   = unlines (map (indent . renderCon) cons)
renderDefs (PTNeg _ _ dess)   = unlines (map (indent . renderDes) dess)
renderDefs (PTFun _ _ _ _ rs) = unlines (map (indent . renderRule) rs)

renderDefinition :: PT -> String
renderDefinition pt = (renderSignature pt) ++ " where\n" ++ (renderDefs pt)

renderProgram :: [PT] -> String
renderProgram p = unlines $ map renderDefinition p