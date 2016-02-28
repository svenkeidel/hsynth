{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Language.Frontend
 ( arr
 , first
 , second
 , (***)
 , (&&&)
 , (>>>)
 , loop
 , init
 , const
 , sconst
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
 , P.Num (..)
 )
where

import           Prelude (Double,Int,Bool)
import qualified Prelude as P

import Control.Category

import           Language.Expression
import           Language.SimpleExpression (SimpleExpr)
import qualified Language.SimpleExpression as S
import           Language.SynArrow
import           Language.Haskell.TH.Syntax


type Signal a b c = SynArrow (SimpleExpr Lift) (Function a) b c

arr :: Syntax (Expr a b -> Expr a c) => Fun (Expr a b) (Expr a c) -> SynArrow i (Function a) b c
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

init :: i b -> SynArrow i a b b
init = Init

const :: b -> Expr a b
const = Const

sconst :: a -> SimpleExpr a
sconst = S.Const

class Internalizable a where
  type Internal a :: *
  internalize :: a -> Internal a

class Externalizable a where
  type External a :: *
  externalize :: External a -> a

type Fun a b = Internal a -> External b
type Syntax a = (Internalizable a, Externalizable a)

instance (Externalizable (SimpleExpr c a), Externalizable (SimpleExpr c b)) => Externalizable (SimpleExpr c (a,b)) where
  type External (SimpleExpr c (a,b)) = (External (SimpleExpr c a), External (SimpleExpr c b))
  externalize (a,b) = S.Inj (externalize a) (externalize b)

instance Externalizable (SimpleExpr c Double) where
  type External (SimpleExpr c Double) = Double
  externalize = S.Const

instance Externalizable (SimpleExpr c Int) where
  type External (SimpleExpr c Int) = Int
  externalize = S.Const

instance Externalizable (SimpleExpr c Bool) where
  type External (SimpleExpr c Bool) = Bool
  externalize = S.Const

instance Externalizable (SimpleExpr c ()) where
  type External (SimpleExpr c ()) = ()
  externalize = S.Const

instance (Internalizable (Expr a b), Internalizable (Expr a c)) => Internalizable (Expr a (b,c)) where
  type Internal (Expr a (b,c)) = (Internal (Expr a b), Internal (Expr a c))
  internalize expr = (internalize (Proj1 expr), internalize (Proj2 expr))

instance (Externalizable (Expr a b), Externalizable (Expr a c)) => Externalizable (Expr a (b,c)) where
  type External (Expr a (b,c)) = (External (Expr a b), External (Expr a c))
  externalize (e1,e2) = Inj (externalize e1) (externalize e2)

instance Internalizable (Expr a ()) where
  type Internal (Expr a ()) = Expr a ()
  internalize = id

instance Externalizable (Expr a ()) where
  type External (Expr a ()) = Expr a ()
  externalize = id

instance Internalizable (Expr a Double) where
  type Internal (Expr a Double) = Expr a Double
  internalize = id

instance Externalizable (Expr a Double) where
  type External (Expr a Double) = Expr a Double
  externalize = id

instance Internalizable (Expr a Int) where
  type Internal (Expr a Int) = Expr a Int
  internalize = id

instance Externalizable (Expr a Int) where
  type External (Expr a Int) = Expr a Int
  externalize = id

instance Internalizable (Expr a Bool) where
  type Internal (Expr a Bool) = Expr a Bool
  internalize = id

instance Externalizable (Expr a Bool) where
  type External (Expr a Bool) = Expr a Bool
  externalize = id

instance (Externalizable a, Internalizable b) => Internalizable (a -> b) where
  type Internal (a -> b) = External a -> Internal b
  internalize f = internalize . f . externalize

instance (Internalizable a, Externalizable b) => Externalizable (a -> b) where
  type External (a -> b) = Internal a -> External b
  externalize f = externalize . f . internalize
