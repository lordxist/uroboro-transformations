module UroboroTransformations.Util.Typed where

import Uroboro.Tree

class Typed a where
  getType :: a -> Type

instance Typed PTCon where
  getType (PTCon _ t _ _) = t

instance Typed PTDes where
  getType (PTDes _ _ _ _ t) = t

instance Typed TQ where
  getType (TQApp t _ _) = t
  getType (TQDes t _ _ _) = t

hasType :: Typed a => Type -> a -> Bool
hasType t tpd = t == (getType tpd)