{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.SimpleExpression where

import Language.SynArrow

data SimpleExpr a where
  Inj :: SimpleExpr a -> SimpleExpr b -> SimpleExpr (a,b)
  Unit :: SimpleExpr ()
  Const :: Double -> SimpleExpr Double
  Fix :: SimpleExpr a

deriving instance Show (SimpleExpr a)

instance Product SimpleExpr where
  unit = Unit
  inj = Inj
  fix = Fix
