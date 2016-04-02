{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Language.Frontend
 ( SynArrow(Arr)
 , arr
 , first
 , second
 , (***)
 , (&&&)
 , (>>>)
 , (<<<)
 , loop
 , loopD
 , unfold
 , scan
 , integral
 , init
 , true
 , false
 , double
 , fromIntegral
 , Num(..)
 , Fractional(..)
 , Floating(..)
 , Category (..)
 , Internalizable (..)
 , Externalizable (..)
 , Fun
 , Signal
 , Syntax
 , Expr
 , SimpleExpr
 , Double
 , Int
 , Bool
 , compile
 , prettyPrint
 , abstractSyntax
 , showSignal
 )
where

import           Prelude hiding ((.),id,init)

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
  (Arr f) -> showParen (d > app_prec)
           $ showString "arr "
           . (showParen True $ showsPrec (app_prec + 1) f)
  (First f) -> showParen (d > app_prec)
             $ showString "first "
             . showsSignal (app_prec + 1) f
  (Compose f g) -> showParen (d > compose_prec)
               $ showsSignal (compose_prec +1) f
               . showString " >>> "
               . showsSignal (d + 1) g
  (Init i) -> showParen (d > app_prec)
            $ showString "init "
            . showsPrec (app_prec + 1) i
  (LoopD i f) -> showParen (d > app_prec)
               $ showString "loopD "
               . showsPrec (app_prec + 1) i
               . showString " "
               . showsPrec (app_prec + 1) f
  (Loop f) -> showParen (d > app_prec)
            $ showString "loop "
            . showsSignal (app_prec + 1) f
  where
    app_prec = 10
    compose_prec = 1

arr :: Syntax (Expr a -> Expr b) => Fun a b -> SynArrow i Function a b
arr = Arr . Function . externalize

first :: SynArrow i a b c -> SynArrow i a (b,d) (c,d)
first = First

second :: SemiArrow a => SynArrow i a b c -> SynArrow i a (d,b) (d,c)
second f = Arr swap >>> first f >>> Arr swap

(***) :: SemiArrow a => SynArrow i a b c -> SynArrow i a b' c' -> SynArrow i a (b,b') (c,c')
f *** g = first f >>> second g

(&&&) :: SemiArrow a => SynArrow i a b c -> SynArrow i a b c' -> SynArrow i a b (c,c')
f &&& g = Arr dup >>> f *** g

loop :: SynArrow i a (b,d) (c,d) -> SynArrow i a b c
loop = Loop

loopD :: Syntax (Expr (b,d) -> Expr (c,d)) => i d -> Fun (b,d) (c,d) -> SynArrow i Function b c
loopD i f = LoopD i (Function (externalize f))

unfold :: (Syntax (Expr c), Syntax (Expr b), Syntax (Expr d))
       => Fun c (b,c) -> SimpleExpr c -> Signal d b
unfold f s = loopD s (\(_,x) -> f x)

scan :: (Syntax (Expr a), Syntax (Expr b), Syntax (Expr (a,b) -> Expr b))
     => Fun (a,b) b -> SimpleExpr b -> Signal a b
scan f s = loopD s (\x -> let y = f x in (y,y))

integral :: Int -> Signal Double Double
integral rate = scan (\(i,x) -> i + x * dt) (externalize 0)
  where
    dt = 1 / fromIntegral rate

init :: i b -> SynArrow i a b b
init = Init

true :: Expr Bool
true = Const True

false :: Expr Bool
false = Const False

double :: Double -> Expr Double
double = Const

class Internalizable a where
  type Internal a :: *
  internalize :: a -> Internal a

class Externalizable a where
  type External a :: *
  externalize :: External a -> a

type Fun a b = Internal (Expr a) -> External (Expr b)
type Syntax a = (Internalizable a, Externalizable a)

instance (Externalizable (SimpleExpr a), Externalizable (SimpleExpr b)) => Externalizable (SimpleExpr (a,b)) where
  type External (SimpleExpr (a,b)) = (External (SimpleExpr a), External (SimpleExpr b))
  externalize (a,b) = S.Inj (externalize a) (externalize b)

instance Externalizable (SimpleExpr Double) where
  type External (SimpleExpr Double) = Double
  externalize = S.Const

instance Externalizable (SimpleExpr Bool) where
  type External (SimpleExpr Bool) = Bool
  externalize = S.Const

instance Externalizable (SimpleExpr ()) where
  type External (SimpleExpr ()) = ()
  externalize _ = S.Unit

instance (Internalizable (Expr a), Internalizable (Expr b)) => Internalizable (Expr (a,b)) where
  type Internal (Expr (a,b)) = (Internal (Expr a), Internal (Expr b))
  internalize expr = (internalize (Proj1 expr), internalize (Proj2 expr))

instance (Externalizable (Expr a), Externalizable (Expr b)) => Externalizable (Expr (a,b)) where
  type External (Expr (a,b)) = (External (Expr a), External (Expr b))
  externalize (e1,e2) = Inj (externalize e1) (externalize e2)

instance Internalizable (Expr ()) where
  type Internal (Expr ()) = Expr ()
  internalize = id

instance Externalizable (Expr ()) where
  type External (Expr ()) = Expr ()
  externalize = id

instance Internalizable (Expr Double) where
  type Internal (Expr Double) = Expr Double
  internalize = id

instance Externalizable (Expr Double) where
  type External (Expr Double) = Expr Double
  externalize = id

instance Internalizable (Expr Int) where
  type Internal (Expr Int) = Expr Int
  internalize = id

instance Externalizable (Expr Int) where
  type External (Expr Int) = Expr Int
  externalize = id

instance Internalizable (Expr Bool) where
  type Internal (Expr Bool) = Expr Bool
  internalize = id

instance Externalizable (Expr Bool) where
  type External (Expr Bool) = Expr Bool
  externalize = id

instance (Externalizable a, Internalizable b) => Internalizable (a -> b) where
  type Internal (a -> b) = External a -> Internal b
  internalize f = internalize . f . externalize

instance (Internalizable a, Externalizable b) => Externalizable (a -> b) where
  type External (a -> b) = Internal a -> External b
  externalize f = externalize . f . internalize
