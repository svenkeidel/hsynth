{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SynArrow where

import Prelude hiding ((.),id,init)
import Control.Category
import Control.Arrow hiding (second)
import Language.Product

-- | The AST of arrow syntax
data SynArrow i a b c where
  Arr :: a b c -> SynArrow i a b c
  First :: SynArrow i a b c -> SynArrow i a (b,d) (c,d)
  Compose :: SynArrow i a b c -> SynArrow i a c d -> SynArrow i a b d
  Init :: i b -> SynArrow i a b b
  LoopD :: i d -> SynArrow i a (b,d) (c,d) -> SynArrow i a b c

second :: Optimizable a => SynArrow i a b c -> SynArrow i a (d,b) (d,c)
second f = Arr swap >>> First f >>> Arr swap

instance Show (SynArrow i a b c) where
  showsPrec d e = case e of
    (Arr _) -> showParen (d > app_prec) $ showString "arr _"
    (First f) -> showParen (d > app_prec)
               $ showString "first "
               . showsPrec (app_prec+1) f
    (Compose f g) -> showParen (d > compose_prec)
                 $ showsPrec (compose_prec+1) f
                 . showString " >>> "
                 . showsPrec (d+1) g
    (Init _) -> showParen (d > app_prec)
              $  showString "init _"
    (LoopD i f) -> showParen (d > app_prec)
                 $ showString "loopD _ "
                 . showsPrec (app_prec+1) f
    where app_prec = 10
          compose_prec = 1

instance Category a => Category (SynArrow i a) where
  id = Arr id
  f . g = Compose g f

instance Arrow a => Arrow (SynArrow i a) where
  arr = Arr . arr
  first = First

class Category a => Optimizable a where
  swap :: a (b,c) (c,b)
  assoc1 :: a ((x,y),z) (x,(y,z))
  assoc2 :: a (x,(y,z)) ((x,y),z)
  (><) :: a b c -> a b' c' -> a (b,b') (c,c')

instance Optimizable (->) where
  swap (x,y) = (y,x)
  assoc1 ((x,y),z) = (x,(y,z))
  assoc2 (x,(y,z)) = ((x,y),z)
  (><) = (***)

optimize :: (Optimizable a, Product i) => SynArrow i a b c -> SynArrow i a b c
optimize a0 =
  case a0 of
    normal@(Arr _) -> normal
    normal@(LoopD _ (Arr _)) -> normal
    (Init i) -> LoopD i (Arr swap)
    (First f) -> single (First (optimize f))
    (Compose f g) -> single (Compose (optimize f) (optimize g))
    (LoopD i f) -> single (LoopD i (optimize f))
  where
    single :: (Optimizable a, Product i) => SynArrow i a b c -> SynArrow i a b c
    single b0 =
        case b0 of
          Compose (Arr f) (Arr g) -> Arr (f >>> g)
          Compose (Arr f) (LoopD i g) -> LoopD i (Arr (f >< id) >>> g)
          Compose (LoopD i f) (Arr g) -> LoopD i (f >>> Arr (g >< id))
          Compose (LoopD i f) (LoopD j g) ->
            LoopD (inj i j) $ assoc' (juggle' (First g) . First f)
          First (Arr f) -> Arr (f >< id)
          LoopD i (LoopD j f) -> LoopD (inj i j) (Arr assoc2 >>> f >>> Arr assoc1)
          a -> a

    assoc' f = Arr assoc2 >>> f >>> Arr assoc1
    juggle' f = Arr juggle >>> f >>> Arr juggle

    juggle :: Optimizable a => a ((b,c),d) ((b,d),c)
    juggle = assoc2 <<< (id >< swap) <<< assoc1
