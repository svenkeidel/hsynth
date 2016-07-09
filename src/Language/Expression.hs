{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Expression where

import           Prelude hiding ((.),id)

import           Control.Category

import           Data.Char (isAlpha)

import           Language.SynArrow
import           Language.Haskell.TH.Syntax

import           Data.Text (Text)
import qualified Data.Text as T

data Expr a where
  Var :: Expr a
  Const :: (Lift a, Show a) => a -> Expr a

  Fun :: Text -> Q (TExp (a -> b)) -> Expr (a -> b)
  App :: Expr (a -> b) -> Expr a -> Expr b

  If :: Expr Bool -> Expr a -> Expr a -> Expr a

  Inj :: Expr a -> Expr b -> Expr (a,b)
  Proj1 :: Expr (a,b) -> Expr a
  Proj2 :: Expr (a,b) -> Expr b

pattern Fun1 f q e1 = App (Fun f q) e1
pattern Fun2 f q e1 e2 = App (App (Fun f q) e1) e2

optimizeExpr :: Expr a -> Expr a
optimizeExpr e = case e of
  Proj1 (Inj e1 _) -> optimizeExpr e1
  Proj2 (Inj _ e2) -> optimizeExpr e2
  Proj1 (If e1 e2 e3) -> optimizeExpr (If e1 (Proj1 e2) (Proj1 e3))
  Proj2 (If e1 e2 e3) -> optimizeExpr (If e1 (Proj2 e2) (Proj2 e3))
  Proj1 e1 -> case optimizeExpr e1 of
    Inj e3 _ -> optimizeExpr e3
    If e1' e2' e3' -> optimizeExpr (If e1' (Proj1 e2') (Proj1 e3'))
    e2 -> Proj1 e2
  Proj2 e2 -> case optimizeExpr e2 of
    Inj _ e3 -> optimizeExpr e3
    If e1' e2' e3' -> optimizeExpr (If e1' (Proj2 e2') (Proj2 e3'))
    e1 -> Proj2 e1
  Inj e1 e2 -> Inj (optimizeExpr e1) (optimizeExpr e2)
  App e1 e2 -> App (optimizeExpr e1) (optimizeExpr e2)
  If e1 e2 e3 -> If (optimizeExpr e1) (optimizeExpr e2) (optimizeExpr e3)
  e1 -> e1

newtype Function a b = Function (Expr a -> Expr b)

instance Show (Function a b) where
  show (Function f) = show $ optimizeExpr $ f Var

instance Category Function where
  id = Function id
  Function f . Function g = Function (f . g)

instance SemiArrow Function where
  swap = Function $ \x -> Inj (Proj2 x) (Proj1 x)
  assoc1 = Function $ \x -> Inj (Proj1 (Proj1 x)) (Inj (Proj2 (Proj1 x)) (Proj2 x))
  assoc2 = Function $ \x -> Inj (Inj (Proj1 x) (Proj1 (Proj2 x))) (Proj2 (Proj2 x))
  Function f >< Function g = Function $ \x -> Inj (f (Proj1 x)) (g (Proj2 x))
  dup = Function $ \x -> Inj x x

fun1 :: Text -> Q (TExp (a -> b)) -> Expr a -> Expr b
fun1 s f e1 = Fun s f `App` e1

fun2 :: Text -> Q (TExp (a -> b -> c)) -> Expr a -> Expr b -> Expr c
fun2 s f e1 e2 = fun1 s f e1 `App` e2

fun3 :: Text -> Q (TExp (a -> b -> c -> d)) -> Expr a -> Expr b -> Expr c -> Expr d
fun3 s f e1 e2 e3 = fun2 s f e1 e2 `App` e3

instance Show (Expr a) where
  showsPrec d e = case e of
    Var -> showString "x"
    Const x -> shows x
    Fun f _ -> showString (T.unpack f)
    App (App (Fun f _) e1) e2 | T.all (not . isAlpha) f ->
      showParen (d > app_prec) $
        showsPrec (app_prec + 1) e1 .
        showString " " .
        showString (T.unpack f) .
        showString " " .
        showsPrec (app_prec + 1) e2
    App e1 e2 -> showParen (d > app_prec)
               $ showsPrec (app_prec + 1) e1
               . showString " "
               . showsPrec (app_prec + 1) e2
    If e1 e2 e3 -> showParen (d > app_prec)
                 $ showString "if "
                 . showsPrec (app_prec + 1) e1
                 . showString " then "
                 . showsPrec (app_prec + 1) e2
                 . showString " else "
                 . showsPrec (app_prec + 1) e3
    Inj e1 e2 -> showParen (d > app_prec)
               $ showString "("
               . showsPrec (app_prec + 1) e1
               . showString ","
               . showsPrec (app_prec + 1) e2
               . showString ")"
    Proj1 expr -> showParen (d > app_prec)
               $ showString "fst "
               . showsPrec (app_prec + 1) expr
    Proj2 expr -> showParen (d > app_prec)
               $ showString "snd "
               . showsPrec (app_prec + 1) expr
    where
      app_prec = 10


instance (Show a, Lift a, Num a) => Num (Expr a) where
  (+) = fun2 "+" [|| (+) ||]
  (-) = fun2 "-" [|| (-) ||]
  (*) = fun2 "*" [|| (*) ||]
  abs = fun1 "abs" [|| abs ||]
  signum = fun1 "signum" [|| signum ||]
  fromInteger = Const . fromIntegral

instance (Show a, Lift a, Fractional a) => Fractional (Expr a) where
  fromRational = Const . fromRational
  (/) = fun2 "/" [|| (/) ||]

instance (Show a, Lift a, Floating a) => Floating (Expr a) where
  pi = Const pi
  exp = fun1 "exp" [|| exp ||]
  log = fun1 "log" [|| exp ||]
  sqrt = fun1 "sqrt" [|| exp ||]
  (**) = fun2 "(**)" [|| (**) ||]
  logBase = fun2 "exp" [|| logBase ||]

  sin = fun1 "sin" [|| sin ||]
  asin = fun1 "asin" [|| asin ||]
  sinh = fun1 "sinh" [|| sinh ||]
  asinh = fun1 "asinh" [|| asinh ||]

  cos = fun1 "cos" [|| cos ||]
  acos = fun1 "acos" [|| acos ||]
  cosh = fun1 "cosh" [|| cosh ||]
  acosh = fun1 "acosh" [|| acosh ||]

  tan = fun1 "tan" [|| tan ||]
  atan = fun1 "atan" [|| atan ||]
  tanh = fun1 "tanh" [|| tanh ||]
  atanh = fun1 "atanh" [|| atanh ||]
