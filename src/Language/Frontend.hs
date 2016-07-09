{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Frontend
 ( SynArrow(Arr), arr, arr', first, second, (***), (&&&), (>>>), (<<<)
 , loop, loopD, init, Category (..)

 , Expr

 , unfold, scan, integral, uncurry, uncurryD, mix

 , true, false, cond, (==), (/=)
 , (<), (<=), (>), (>=), (>+<), (>*<)

 , double, fromIntegral , Num(..), Fractional(..), Floating(..), RealFrac(..)
 , int

 , Syntax (..)
 , SimpleSyntax (..)

 , Signal, SimpleExpr, Double, Int, Bool

 , compile, prettyPrint, abstractSyntax, showSignal

 , undefined
 )
where

import           Prelude hiding ((.),id,init,uncurry,(==),(/=),(<),(<=),(>=),(>))

import qualified Prelude as P

import           Control.Category

import           Language.Expression
import           Language.SimpleExpression (SimpleExpr)
import qualified Language.SimpleExpression as S
import           Language.SynArrow
import           Language.CodeGen.TH (compile,prettyPrint,abstractSyntax)

type Signal a b = SynArrow SimpleExpr Function a b

showSignal :: Signal a b -> String
showSignal sig = showsSignal 0 sig ""

showsSignal :: Int -> Signal a b -> ShowS
showsSignal d e = case e of
  (Arr f) -> showParen (d P.> app_prec)
           $ showString "arr "
           . (showParen True $ showsPrec (app_prec + 1) f)
  (First f) -> showParen (d P.> app_prec)
             $ showString "first "
             . showsSignal (app_prec + 1) f
  (Compose f g) -> showParen (d P.> compose_prec)
               $ showsSignal (compose_prec +1) f
               . showString " >>> "
               . showsSignal (d + 1) g
  (Init i) -> showParen (d P.> app_prec)
            $ showString "init "
            . showsPrec (app_prec + 1) i
  (LoopD i f) -> showParen (d P.> app_prec)
               $ showString "loopD "
               . showsPrec (app_prec + 1) i
               . showString " "
               . showsPrec (app_prec + 1) f
  (Loop f) -> showParen (d P.> app_prec)
            $ showString "loop "
            . showsSignal (app_prec + 1) f
  where
    app_prec = 10
    compose_prec = 1

arr :: (Syntax a, Syntax b) => (a -> b) -> Signal (Internal a) (Internal b)
arr = arr' . lowerFun

arr' :: (Expr a -> Expr b) -> Signal a b
arr' = Arr . Function

first :: Signal b c -> Signal (b,d) (c,d)
first = First

second :: Signal b c -> Signal (d,b) (d,c)
second f = Arr swap >>> first f >>> Arr swap

(***) :: Signal b c -> Signal b' c' -> Signal (b,b') (c,c')
f *** g = first f >>> second g
infixr 3 ***

(&&&) :: Signal b c -> Signal b c' -> Signal b (c,c')
f &&& g = Arr dup >>> f *** g
infixr 3 &&&

loop :: Signal (b,d) (c,d) -> Signal b c
loop = Loop

loopD :: (Syntax (b,d), Syntax (c,d), SimpleSyntax (Internal d))
      => Internal d -> ((b,d) -> (c,d)) -> Signal (Internal b) (Internal c)
loopD i f = LoopD (simpleExpr i) (Function (lowerFun f))

unfold :: (Syntax c, Syntax b, SimpleSyntax (Internal c))
       => (c -> (b,c)) -> Internal c -> Signal () (Internal b)
unfold f s = loopD s (\((),x) -> f x)

scan :: (Syntax b, Syntax (a,b), SimpleSyntax (Internal b))
     => ((a,b) -> b) -> Internal b -> Signal (Internal a) (Internal b)
scan f s = loopD s (\x -> let y = f x in (y,y))

integral :: Int -> Signal Double Double
integral rate = scan (\(i,x) -> i + x * double dt) 0
  where
    dt = 1 / fromIntegral rate

init :: i b -> SynArrow i a b b
init = Init

uncurry :: Syntax (a,b) => (a -> b -> c) -> Expr (Internal a,Internal b) -> c
uncurry f = P.uncurry f . externalize

mix :: (Expr Double -> Expr Double -> Expr Double) -> Signal a Double -> Signal a Double -> Signal a Double
mix f a b = (a &&& b) >>> arr (\(a',b') -> f a' b')

(>+<) :: Signal a Double -> Signal a Double -> Signal a Double
(>+<) = mix (+)
infixl 6 >+<

(>*<) :: Signal a Double -> Signal a Double -> Signal a Double
(>*<) = mix (*)
infixl 7 >*<

uncurryD :: (Expr Double -> Expr Double -> Expr Double) -> Expr (Double,Double) -> Expr Double
uncurryD = uncurry

true :: Expr Bool
true = Const True

false :: Expr Bool
false = Const False

double :: Double -> Expr Double
double = Const

int :: Int -> Expr Int
int = Const

cond :: Syntax a => Expr Bool -> a -> a -> a
cond a b c = externalize (If a (internalize b) (internalize c))

(==) :: Eq a => Expr a -> Expr a -> Expr Bool
(==) = fun2 "==" [|| (P.==) ||]
infix 4 ==

(/=) :: Eq a => Expr a -> Expr a -> Expr Bool
(/=) = fun2 "==" [|| (P./=) ||]
infix 4 /=

(<) :: Ord a => Expr a -> Expr a -> Expr Bool
(<) = fun2 "<" [|| (P.<) ||]
infix 4 <

(<=) :: Ord a => Expr a -> Expr a -> Expr Bool
(<=) = fun2 "<=" [|| (P.<=) ||]
infix 4 <=

(>) :: Ord a => Expr a -> Expr a -> Expr Bool
(>) = fun2 ">" [|| (P.>) ||]
infix 4 >

(>=) :: Ord a => Expr a -> Expr a -> Expr Bool
(>=) = fun2 ">=" [|| (P.>=) ||]
infix 4 >=

class SimpleSyntax a where
  simpleExpr :: a -> SimpleExpr a

instance (SimpleSyntax a, SimpleSyntax b) => (SimpleSyntax (a,b)) where
  simpleExpr (a,b) = S.Inj (simpleExpr a) (simpleExpr b)

instance SimpleSyntax Double where
  simpleExpr = S.Const

instance SimpleSyntax Int where
  simpleExpr = S.Const

instance SimpleSyntax Bool where
  simpleExpr = S.Const

instance SimpleSyntax () where
  simpleExpr _ = S.Const ()

class Syntax a where
  type Internal a :: *
  internalize :: a -> Expr (Internal a)
  externalize :: Expr (Internal a) -> a

instance (Syntax a, Syntax b) => Syntax (a,b) where
  type Internal (a,b) = (Internal a, Internal b)
  -- internalize :: (a,b) -> Expr (Internal a, Internal b)
  internalize (e1,e2) = Inj (internalize e1) (internalize e2)
  -- externalize :: Expr (Internal a, Internal b) -> (a,b)
  externalize expr = (externalize (Proj1 expr), externalize (Proj2 expr))

instance Syntax (Expr ()) where
  type Internal (Expr ()) = ()
  internalize = id
  externalize = id

instance Syntax () where
  type Internal () = ()
  internalize () = Const ()
  externalize _ = ()

instance Syntax (Expr Double) where
  type Internal (Expr Double) = Double
  internalize = id
  externalize = id

instance Syntax (Expr Int) where
  type Internal (Expr Int) = Int
  internalize = id
  externalize = id

instance Syntax (Expr Bool) where
  type Internal (Expr Bool) = Bool
  internalize = id
  externalize = id

lowerFun :: (Syntax a, Syntax b) => (a -> b) -> (Expr (Internal a) -> Expr (Internal b))
lowerFun f = internalize . f . externalize

-- liftFun :: (Syntax a, Syntax b) => (Expr (Internal a) -> Expr (Internal b)) -> (a -> b)
-- liftFun f = externalize . f . internalize
