{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Expression where

import Prelude hiding ((.),id)

import Control.Category

import Language.Function
import Language.SynArrow
import Language.Haskell.TH.Syntax

data Expr a b where
  Var :: Expr a b
  Const :: Lift b => b -> Expr a b

  Fun :: Fun (b -> c) -> Expr a (b -> c)
  App :: Expr a (b -> c) -> Expr a b -> Expr a c

  Inj :: Expr a b -> Expr a c -> Expr a (b,c)
  Proj1 :: Expr a (b,c) -> Expr a b
  Proj2 :: Expr a (b,c) -> Expr a c

--deriving instance (Show (Expr a b))

coerceExpr :: Expr a b -> Expr a' b
coerceExpr expr = case expr of
  Var -> Var
  Const c -> Const c
  Fun f -> Fun f
  App e1 e2 -> App (coerceExpr e1) (coerceExpr e2)
  Inj e1 e2 -> Inj (coerceExpr e1) (coerceExpr e2)
  Proj1 e1 -> Proj1 (coerceExpr e1)
  Proj2 e2 -> Proj2 (coerceExpr e2)

pattern Fun1 f e1 = App (Fun f) e1
pattern Fun2 f e1 e2 = App (App (Fun f) e1) e2

optimizeExpr :: Expr a b -> Expr a b
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

newtype Function a b c = Function (Expr a b -> Expr a c)

coerceFun :: Function a b c -> Function a' b c
coerceFun (Function f) = Function (coerceExpr . f . coerceExpr)

instance Category (Function a) where
  id = Function id
  Function f . Function g = Function (f . g)

instance SemiArrow (Function a) where
  swap = Function $ \x -> Inj (Proj2 x) (Proj1 x)
  assoc1 = Function $ \x -> Inj (Proj1 (Proj1 x)) (Inj (Proj2 (Proj1 x)) (Proj2 x))
  assoc2 = Function $ \x -> Inj (Inj (Proj1 x) (Proj1 (Proj2 x))) (Proj2 (Proj2 x))
  Function f >< Function g = Function $ \x -> Inj (f (Proj1 x)) (g (Proj2 x))
  dup = Function $ \x -> Inj x x

fun1 :: Fun (b -> c) -> Expr a b -> Expr a c
fun1 f e1 = Fun f `App` e1

fun2 :: Fun (b -> c -> d) -> Expr a b -> Expr a c -> Expr a d
fun2 f e1 e2 = fun1 f e1 `App` e2

instance (Lift b,Num b) => Num (Expr a b) where
  (+) = fun2 Add
  (-) = fun2 Sub
  (*) = fun2 Mult
  abs = fun1 Abs
  signum = fun1 Signum
  fromInteger = Const . fromInteger
