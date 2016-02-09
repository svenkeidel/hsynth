{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
module Language.DSP where

import Prelude hiding ((.),id,init)
import Control.Category
import Control.Arrow

-- | The AST of arrow syntax
data SynArrow a b c where
  Arr :: a b c -> SynArrow a b c
  First :: SynArrow a b c -> SynArrow a (b,d) (c,d)
  Second :: SynArrow a b c -> SynArrow a (d,b) (d,c)
  Compose :: SynArrow a b c -> SynArrow a c d -> SynArrow a b d
  Init :: b -> SynArrow a b b
  Loop :: SynArrow a (b,d) (c,d) -> SynArrow a b c

instance Category a => Category (SynArrow a) where
  id = Arr id
  f . g = Compose g f

instance Arrow a => Arrow (SynArrow a) where
  arr = Arr . arr
  first = First
  second = Second

class ArrowInit a where
  init :: b -> a b b

instance ArrowInit (SynArrow a) where
  init = Init

instance Arrow a => ArrowLoop (SynArrow a) where
  loop = Loop

class (Arrow a, ArrowInit a, ArrowLoop a) => Signal a

instance Arrow a => Signal (SynArrow a)

pattern LoopB i f = Loop (Compose f (Second (Second (Init i))))

optimize :: Optimizable a => SynArrow a b c -> SynArrow a b c
optimize a0 =
  case a0 of
    (Init i) -> single (Init i)
    (First f) -> single (First (optimize f))
    (Second f) -> single (Second (optimize f))
    (Compose f g) -> single (Compose (optimize f) (optimize g))
    (Loop f) -> single (Loop (optimize f))
    (LoopB i f) -> optimize (LoopB i (single f))
    a -> a
  where
    single :: Optimizable a => SynArrow a b c -> SynArrow a b c
    single b0 =
        case b0 of
          Loop f -> LoopB () (Arr assoc2 >>> First f >>> Arr assoc1)
          Init i -> LoopB i (Arr swap <<< Arr juggle <<< Arr swap)
          Compose (Arr f) (Arr g) -> Arr (f >>> g)
          First (Arr f) -> Arr (f >< id)
          Second (Arr f) -> Arr (id >< f)
          Compose h (LoopB i f) -> LoopB i (First h >>> f)
          Compose (LoopB i f) (Arr g) -> LoopB i (f >>> First (Arr g))
          LoopB i (LoopB j f) -> LoopB (i,j) (Arr shuffle1 >>> f >>> Arr shuffle2)
          First (LoopB i f) -> LoopB i (Arr juggle >>> First f >>> Arr juggle)
          a -> a

    juggle :: Optimizable a => a ((b,c),d) ((b,d),c)
    juggle = assoc2 <<< (id >< swap) <<< assoc1

    transpose :: Optimizable a => a ((b,c),(d,e)) ((b,d),(c,e))
    transpose = assoc1 <<< (juggle >< id) <<< assoc2

    shuffle1 :: Optimizable a => a (b,((c,d),(e,f)))((b,(c,e)),(d,f))
    shuffle1 = assoc2 <<< (id >< transpose)

    shuffle2 :: Optimizable a => a ((b,(c,d)),(e,f)) (b,((c,e),(d,f)))
    shuffle2 = (id >< transpose) <<< assoc1

class Category a => Optimizable a where
  swap :: a (b,c) (c,b)
  assoc1 :: a ((x,y),z) (x,(y,z))
  assoc2 :: a (x,(y,z)) ((x,y),z)
  (><) :: a b c -> a b' c' -> a (b,b') (c,c')

instance Optimizable (->) where
  swap = arr $ \(x,y) -> (y,x)
  assoc1 = arr $ \((x,y),z) -> (x,(y,z))
  assoc2 = arr $ \(x,(y,z)) -> ((x,y),z)
  (><) = (***)

data Expr a where
  Var :: Int -> Expr a
  Const :: Double -> Expr Double

  Add :: Expr Double -> Expr Double -> Expr Double
  Mult :: Expr Double -> Expr Double -> Expr Double
  Sub :: Expr Double -> Expr Double -> Expr Double
  Div :: Expr Double -> Expr Double -> Expr Double
  Abs :: Expr Double -> Expr Double
  Signum :: Expr Double -> Expr Double

  Sin :: Expr Double -> Expr Double
  Cos :: Expr Double -> Expr Double

deriving instance (Show (Expr a))

instance Num (Expr Double) where
  (+) = Add
  (-) = Sub
  (*) = Mult
  abs = Abs
  signum = Signum
  fromInteger = Const . fromInteger

unfold :: Signal a => (c -> (b,c)) -> c -> a d b
unfold f s = loop (arr snd >>> arr f >>> second (init s))

type Frequency = Double
type Rate = Int

sinA :: Signal a => Frequency -> Rate -> a d (Expr Double)
sinA freq rate =
  let omh  = 2 * pi * freq / fromIntegral rate
      i    = sin omh
      c    = 2 * cos omh
  in unfold (\(y1,y2) -> let y = Const c * y1 - y2 in (y2,(y,y1))) (Const i,0)
