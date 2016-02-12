{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Expression where

import Prelude hiding ((.),id)

import Control.Category
import Language.Optimizable
import Language.Signal

data Expr a where
  Var :: Expr a
  Const :: Double -> Expr Double

  Add :: Expr Double -> Expr Double -> Expr Double
  Mult :: Expr Double -> Expr Double -> Expr Double
  Sub :: Expr Double -> Expr Double -> Expr Double
  Div :: Expr Double -> Expr Double -> Expr Double
  Abs :: Expr Double -> Expr Double
  Signum :: Expr Double -> Expr Double

  Sin :: Expr Double -> Expr Double
  Cos :: Expr Double -> Expr Double

  Inj :: Expr a -> Expr b -> Expr (a,b)
  Proj1 :: Expr (a,b) -> Expr a
  Proj2 :: Expr (a,b) -> Expr b

deriving instance (Show (Expr a))

newtype Function a b = Function (Expr a -> Expr b)

instance Category Function where
  id = Function id
  Function f . Function g = Function (f . g)

instance Optimizable Function where
  swap = Function $ \x -> Inj (Proj2 x) (Proj1 x)
  assoc1 = Function $ \x -> Inj (Proj1 (Proj1 x)) (Inj (Proj2 (Proj1 x)) (Proj2 x))
  assoc2 = Function $ \x -> Inj (Inj (Proj1 x) (Proj1 (Proj2 x))) (Proj2 (Proj2 x))
  Function f >< Function g = Function $ \x -> Inj (f (Proj1 x)) (g (Proj2 x))

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


-- data Expr' where
--   Var' :: Var a -> Expr'
--   Const' :: Double -> Expr'

--   Add' :: Expr' -> Expr' -> Expr'
--   Mult' :: Expr' -> Expr' -> Expr'
--   Sub' :: Expr' -> Expr' -> Expr'
--   Div' :: Expr' -> Expr' -> Expr'
--   Abs' :: Expr' -> Expr'
--   Signum' :: Expr' -> Expr'

--   Sin' :: Expr' -> Expr'
--   Cos' :: Expr' -> Expr'

--   Inj' :: Expr' -> Expr' -> Expr'
--   Proj1' :: Expr' -> Expr'
--   Proj2' :: Expr' -> Expr'

-- lower :: Expr a -> Expr'
-- lower e =
--   case e of
--     Var v -> Var' v
--     Const c -> Const' c
--     Add e1 e2 -> Add' (lower e1) (lower e2)
--     Mult e1 e2 -> Mult' (lower e1) (lower e2)
--     Sub e1 e2 -> Sub' (lower e1) (lower e2)
--     Div e1 e2 -> Div' (lower e1) (lower e2)
--     Abs e1 -> Abs' (lower e1)
--     Signum e1 -> Signum' (lower e1)

--     Sin e1 -> Sin' (lower e1)
--     Cos e1 -> Cos' (lower e1)

--     Inj e1 e2 -> Inj' (lower e1) (lower e2)
--     Proj1 e1 -> Proj1' (lower e1)
--     Proj2 e1 -> Proj2' (lower e1)
