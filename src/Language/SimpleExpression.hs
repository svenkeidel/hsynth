{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Language.SimpleExpression where

import Language.SynArrow
import Language.Haskell.TH.Syntax

data SimpleExpr a where
  Inj :: SimpleExpr a -> SimpleExpr b -> SimpleExpr (a,b)
  Const :: (Lift a, Show a) => a -> SimpleExpr a
  Unit :: SimpleExpr ()
  Fix :: SimpleExpr a

deriving instance Show (SimpleExpr a)

instance Product SimpleExpr where
  unit = Unit
  inj = Inj
  fix = Fix

data CompressedSimpleExpr a where
  CInj :: CompressedSimpleExpr a -> CompressedSimpleExpr b -> CompressedSimpleExpr (a,b)
  CConst :: (Lift a, Show a) => a -> CompressedSimpleExpr a
  CUnit :: CompressedSimpleExpr ()

data Decide f where
  Yes :: f a -> Decide f
  No :: Decide f

mapDecide :: (forall a. f a -> g b) -> Decide f -> Decide g
mapDecide f (Yes y) = Yes $ f y
mapDecide _ No = No

merge :: (forall a b. f a -> f b -> Decide f) -> Decide f -> Decide f -> Decide f
merge f d1 d2 = case (d1,d2) of
 (No,No) -> No
 (No, Yes d2') -> Yes d2'
 (Yes d1', No) -> Yes d1'
 (Yes d1',Yes d2') -> f d1' d2'

compressSimpleExpr :: SimpleExpr a -> Decide CompressedSimpleExpr
compressSimpleExpr expr = case expr of
  Unit -> Yes CUnit
  Const c -> Yes (CConst c)
  Fix -> No
  Inj e1 e2 -> merge (\e1' e2' -> Yes (CInj e1' e2')) (compressSimpleExpr e1) (compressSimpleExpr e2)
