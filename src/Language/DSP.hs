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
import Control.Monad.State

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

data SynArrow b c where
  Arr :: (b -> c) -> SynArrow b c
  First :: SynArrow b c -> SynArrow (b,d) (c,d)
  Second :: SynArrow b c -> SynArrow (d,b) (d,c)
  Compose :: SynArrow b c -> SynArrow c d -> SynArrow b d
  Init :: b -> SynArrow b b
  Loop :: SynArrow (b,d) (c,d) -> SynArrow b c

instance Category SynArrow where
  id = Arr id
  f . g = Compose g f

instance Arrow SynArrow where
  arr = Arr
  first = First
  second = Second

class ArrowInit a where
  init :: b -> a b b

instance ArrowInit SynArrow where
  init = Init

instance ArrowLoop SynArrow where
  loop = Loop

class (Arrow a, ArrowInit a, ArrowLoop a) => Signal a

instance Signal SynArrow

pattern LoopB i f = Loop (Compose (Arr f) (Second (Second (Init i))))

optimize :: SynArrow a b -> SynArrow a b
optimize a0 =
  case a0 of
    (Init i) -> LoopB i id
    (First f) -> single (First (optimize f))
    (Second f) -> single (Second (optimize f))
    (Compose f g) -> single (Compose (optimize f) (optimize g))
    (Loop f) -> single (Loop (optimize f))
    a -> a
  where
    single :: SynArrow b c -> SynArrow b c
    single b0 =
        case b0 of
          Compose (Arr f) (Arr g) -> Arr (f >>> g)
          Compose (Arr f) (LoopB i g) -> LoopB i (g . first f)
          Compose (LoopB i f) (Arr g) -> LoopB i (first g . f)
          --Compose (LoopB i f) (Arr g) -> LoopB i (first g . f)
          Compose (LoopB i f) (LoopB j g) -> _ --LoopB (i,j) (assoc (juggle (first g) . first f))
          First (Arr f) -> Arr (first f)
          Second (Arr f) -> Arr (second f)
          First (LoopB i f) -> LoopB i (juggle (first f))
          Second (LoopB i f) -> LoopB i (juggle2 f)
          Loop f -> LoopB () (arr assoc2 >>> (_ (first f)) >>> arr assoc1)
          Loop (LoopB i f) -> LoopB i (trace (juggle f))
          a -> a

    trace f b = let (c, d) = f (b, d) in c

    swap :: (a,b) -> (b,a)
    swap (x,y) = (y,x)

    assoc f = assoc1 . f . assoc2

    assoc1 :: ((x,y),z) -> (x,(y,z))
    assoc1 ((x,y),z) = (x,(y,z))

    assoc2 :: (x,(y,z)) -> ((x,y),z)
    assoc2 (x,(y,z)) = ((x,y),z)

    juggle f = juggle1 . f . juggle1

    juggle1 :: ((x,y),z) -> ((x,z),y)
    juggle1 ((x,y),z) = ((x,z),y)

    juggle2 :: ((a,c) -> (b,c)) -> ((d,a),c) -> ((d,b),c)
    juggle2 f ((d,a),c) =
        let (b',c') = f (a,c)
        in ((d,b'),c')

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

type Gen a = State Int a

fresh :: Gen Int
fresh = do
  modify (+1)
  get

class GenExpr e where
  gen :: Gen e

instance GenExpr (Expr a) where
  gen = Var <$> fresh

instance (GenExpr a, GenExpr b) => GenExpr (a,b) where
  gen = do
    a <- gen
    b <- gen
    return (a,b)

-- data Compiled a b where
--   Pure :: (a -> b) -> Compiled a b
--   Stateful :: ((a,c) -> (b,c)) -> c -> Compiled a b
--   Fail :: Compiled a b

-- instance Show (Compiled a b) where
--   show (Pure {}) = "Pure"
--   show (Stateful {}) = "Stateful"
--   show Fail = "Fail"

-- compile :: SynArrow a b -> Compiled a b
-- compile a = case optimize a of
--   Arr f -> Pure f
--   LoopB i f -> Stateful f i
--   _ -> Fail
