{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Function where

data Fun a where
  Add :: Num a => Fun (a -> a -> a)
  Mult :: Num a => Fun (a -> a -> a)
  Sub :: Num a => Fun (a -> a -> a)
  Div :: Fractional a => Fun (a -> a -> a)
  Abs :: Num a => Fun (a -> a)
  Signum :: Num a => Fun (a -> a)
  Sin :: Fun (Double -> Double)
  Cos :: Fun (Double -> Double)

deriving instance Show (Fun a)
