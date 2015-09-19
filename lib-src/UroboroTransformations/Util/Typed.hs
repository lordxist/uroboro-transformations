module UroboroTransformations.Util.Typed where

import Uroboro.Tree

class Typed a where
  getType :: a -> Type

instance Typed PTCon where
  getType (PTCon _ t _ _) = t

instance Typed PTDes where
  getType (PTDes _ t _ _ _) = t

instance Typed TQ where
  getType (TQApp t _ _) = t
  getType (TQDes t _ _ _) = t

instance Typed TExp where
  getType (TVar t _) = t
  getType (TApp t _ _) = t
  getType (TDes t _ _ _) = t
  getType (TCon t _ _) = t

hasType :: Typed a => Type -> a -> Bool
hasType t tpd = t == (getType tpd)

class TypeAssociated a where
  getAssociatedType :: a -> Type

instance TypeAssociated PTCon where
  getAssociatedType (PTCon _ t _ _) = t

instance TypeAssociated PTDes where
  getAssociatedType (PTDes _ _ _ _ t) = t

belongsToType :: TypeAssociated a => Type -> a -> Bool
belongsToType t ta = t == (getAssociatedType ta)
