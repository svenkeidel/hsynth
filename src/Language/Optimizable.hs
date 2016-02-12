module Language.Optimizable where

import Control.Category
import Control.Arrow

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
