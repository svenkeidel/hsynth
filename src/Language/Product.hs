module Language.Product where

class Product m where
  unit :: m ()
  inj :: m a -> m b -> m (a,b)

