{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Expression where

import Prelude hiding ((.),id)

import Control.Category

import Language.Function
import Language.SynArrow

data Expr a where
  Var :: Expr a
  Double :: Double -> Expr Double
  Bool :: Bool -> Expr Bool

  Fun :: Fun (a -> b) -> Expr (a -> b)
  App :: Expr (a -> b) -> Expr a -> Expr b

  Inj :: Expr a -> Expr b -> Expr (a,b)
  Proj1 :: Expr (a,b) -> Expr a
  Proj2 :: Expr (a,b) -> Expr b

deriving instance (Show (Expr a))

pattern Fun1 f e1 = App (Fun f) e1
pattern Fun2 f e1 e2 = App (App (Fun f) e1) e2

optimizeExpr :: Expr a -> Expr a
optimizeExpr e = case e of
  Proj1 (Inj e1 _) -> optimizeExpr e1
  Proj2 (Inj _ e2) -> optimizeExpr e2
  Proj1 e1 -> case optimizeExpr e1 of
    Inj e3 _ -> optimizeExpr e3
    e2 -> Proj1 e2
  Proj2 e2 -> case optimizeExpr e2 of
    Inj _ e3 -> optimizeExpr e3
    e1 -> Proj2 e1
  Inj e1 e2 -> Inj (optimizeExpr e1) (optimizeExpr e2)
  App e1 e2 -> App (optimizeExpr e1) (optimizeExpr e2)
  e1 -> e1

newtype Function a b = Function (Expr a -> Expr b)

instance Show (Function a b) where
  show (Function f) = show $ f Var

instance Category Function where
  id = Function id
  Function f . Function g = Function (f . g)

instance SemiArrow Function where
  swap = Function $ \x -> Inj (Proj2 x) (Proj1 x)
  assoc1 = Function $ \x -> Inj (Proj1 (Proj1 x)) (Inj (Proj2 (Proj1 x)) (Proj2 x))
  assoc2 = Function $ \x -> Inj (Inj (Proj1 x) (Proj1 (Proj2 x))) (Proj2 (Proj2 x))
  Function f >< Function g = Function $ \x -> Inj (f (Proj1 x)) (g (Proj2 x))
  dup = Function $ \x -> Inj x x

fun1 :: Fun (a -> b) -> Expr a -> Expr b
fun1 f e1 = Fun f `App` e1

fun2 :: Fun (a -> b -> c) -> Expr a -> Expr b -> Expr c
fun2 f e1 e2 = fun1 f e1 `App` e2
