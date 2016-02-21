{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.SimpleExpression where

import Language.Product

data SimpleExpr a where
  Inj :: SimpleExpr a -> SimpleExpr b -> SimpleExpr (a,b)
  Unit :: SimpleExpr ()
  Const :: Double -> SimpleExpr Double

deriving instance Show (SimpleExpr a)

instance Product SimpleExpr where
  unit = Unit
  inj = Inj
