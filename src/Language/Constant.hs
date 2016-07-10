module Language.Constant where

import Language.Haskell.TH.Syntax

class (Lift a,Show a) => Constant a where
  constantType :: a -> Type

instance Constant Bool where
  constantType _ = ConT (mkName "Bool")

instance Constant Int where
  constantType _ = ConT (mkName "Int")

instance Constant Double where
  constantType _ = ConT (mkName "Double")

instance Constant () where
  constantType _ = TupleT 0
