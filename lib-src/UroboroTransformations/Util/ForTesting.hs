module UroboroTransformations.Util.ForTesting where

import Uroboro.Tree
import Uroboro.Parser(parseFile)

import qualified UroboroTransformations.Util.VariableSchema as Schema

import Data.Either(rights)

import Data.Set (Set, fromList)

standardize :: [PT] -> Set PT
standardize = fromList . (map Schema.renameVariables)

parse :: String -> [PT]
parse s = let ptsOrError = parseFile "here" s in
            ((rights [ptsOrError]) !! 0)

instance Eq PTCon where
  (PTCon _ t id ts) == (PTCon _ t' id' ts') = (t == t') && (id == id') && (ts == ts')

instance Eq PTDes where
  (PTDes _ t id ts dt) == (PTDes _ t' id' ts' dt') = (t == t') && (id == id') && (ts == ts') && (dt == dt')

instance Ord Type where
  t <= t' = (show t) <= (show t')

instance Ord PTCon where
  (PTCon _ _ id _) <= (PTCon _ _ id' _) = id <= id'

instance Ord PTDes where
  (PTDes _ _ id _ _) <= (PTDes _ _ id' _ _) = id <= id'

instance Eq PT where
  (PTFun _ id ts t rs) == (PTFun _ id' ts' t' rs') = (id == id') && (ts == ts') && (t == t') && ((fromList rs) == (fromList rs'))
  (PTPos _ t cs) == (PTPos _ t' cs') = (t == t') && ((fromList cs) == (fromList cs'))
  (PTNeg _ t ds) == (PTNeg _ t' ds') = (t == t') && ((fromList ds) == (fromList ds'))
  _ == _ = False

instance Ord PTRule where
  (PTRule _ pq _) <= (PTRule _ pq' _) = pq <= pq'

instance Ord PT where
  (PTFun _ id _ _ rs) <= (PTFun _ id' _ _ rs') = id <= id'
  (PTFun _ id _ _ _) <= (PTPos _ t' _) = id <= (show t')
  (PTFun _ id _ _ _) <= (PTNeg _ t' _) = id <= (show t')
  (PTPos _ t _) <= (PTFun _ id' _ _ _) = (show t) <= id'
  (PTPos _ t _) <= (PTPos _ t' _) = t <= t'
  (PTPos _ t _) <= (PTNeg _ t' _) = t <= t'
  (PTNeg _ t _) <= (PTFun _ id' _ _ _) = (show t) <= id'
  (PTNeg _ t _) <= (PTPos _ t' _) = t <= t'
  (PTNeg _ t _) <= (PTNeg _ t' _) = t <= t'
