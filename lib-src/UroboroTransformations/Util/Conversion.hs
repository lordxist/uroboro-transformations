module UroboroTransformations.Util.Conversion where

import Uroboro.Tree

import UroboroTransformations.Util

tpToPP :: TP -> PP
tpToPP (TPVar t id) = PPVar dummyLocation id
tpToPP (TPCon t id tps) = PPCon dummyLocation id (map tpToPP tps)

tqToPQ :: TQ -> PQ
tqToPQ (TQApp t id tps) = PQApp dummyLocation id (map tpToPP tps)
tqToPQ (TQDes t id tps tq) = PQDes dummyLocation id (map tpToPP tps) (tqToPQ tq) 

tExpToPExp :: TExp -> PExp
tExpToPExp (TApp t id tExps) = PApp dummyLocation id (map tExpToPExp tExps)
tExpToPExp (TDes t id tExps tExp) = PDes dummyLocation id (map tExpToPExp tExps) (tExpToPExp tExp)
tExpToPExp (TVar t id) = PVar dummyLocation id
tExpToPExp (TCon t id tExps) = PApp dummyLocation id (map tExpToPExp tExps)

tExpToTQ :: TExp -> TQ
tExpToTQ (TApp t id tExps) = TQApp t id (map tExpToTP tExps)
tExpToTQ (TDes t id tExps tExp) = TQDes t id (map tExpToTP tExps) (tExpToTQ tExp)

tExpToTP :: TExp -> TP
tExpToTP (TVar t id) = TPVar t id
tExpToTP (TCon t id tExps) = TPCon t id (map tExpToTP tExps)

tpToTExp :: TP -> TExp
tpToTExp (TPVar t id) = TVar t id
tpToTExp (TPCon t id tps) = TApp t id (map tpToTExp tps)
