module UroboroTransformations.Util.Equality where

import Uroboro.Tree

instance Eq PP where
  (PPVar _ id) == (PPVar _ id') = True
  (PPCon _ id pps) == (PPCon _ id' pps') = (id == id') && (pps == pps')
  _ == _ = False

instance Eq PQ where
  (PQApp _ id pps) == (PQApp _ id' pps') = (id == id') && (pps == pps')
  (PQDes _ id pps pq) == (PQDes _ id' pps' pq') = (id == id') && (pps == pps') && (pq == pq')
  _ == _ = False

instance Eq PExp where
  (PApp _ id pexps) == (PApp _ id' pexps') = (id == id') && (pexps == pexps')
  (PVar _ id) == (PVar _ id') = id == id'
  (PDes _ id pexps pexp) == (PDes _ id' pexps' pexp') = (id == id') && (pexps == pexps') && (pexp == pexp')
  _ == _ = False

instance Eq PTRule where
  (PTRule _ pq pexp) == (PTRule _ pq' pexp') = (pq == pq') && (pexp == pexp')
