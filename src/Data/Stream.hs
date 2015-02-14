module Data.Stream where

import Prelude hiding ((!!),iterate,zipWith,tail,repeat,take)

data Stream a = Cons !a (Stream a)

(<:>) :: a -> Stream a -> Stream a
(<:>) = Cons
infixr 5 <:>

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Num n => Num (Stream n) where
  (+) = zipWith (+)
  (*) = zipWith (*)
  (-) = zipWith (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = repeat . fromInteger

instance Fractional n => Fractional (Stream n) where
  (/) = zipWith (/)
  recip = fmap recip
  fromRational = repeat . fromRational

instance Floating n => Floating (Stream n) where
  pi = repeat pi
  exp = fmap exp
  sqrt = fmap sqrt
  log = fmap log
  (**) = zipWith (**)
  logBase = zipWith logBase
  sin = fmap sin
  tan = fmap tan
  cos = fmap cos
  asin = fmap asin
  atan = fmap atan
  acos = fmap acos
  sinh = fmap sinh
  tanh = fmap tanh
  cosh = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

switch :: Int -> Stream a -> Stream a -> Stream a
switch 0 _           ys = ys
switch n (Cons x xs) ys = Cons x (switch (n-1) xs ys)

scan :: (a -> b -> a) -> a -> Stream b -> Stream a
scan f a (Cons b bs) = Cons a (scan f (f a b) bs)

unfold :: (s -> (a,s)) -> s -> Stream a
unfold f s0 =
  let (a,s) = f s0
  in Cons a (unfold f s)

iterate :: (a -> a) -> a -> Stream a
iterate f a = Cons a (iterate f (f a))

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipWith f as bs)

tail :: Stream a -> Stream a
tail (Cons _ as) = as

repeat :: a -> Stream a
repeat a = Cons a (repeat a)

(!!) :: Stream a -> Int -> a
Cons a _ !! 0 = a
Cons _ s !! n = s !! (n-1)
infixl 9 !!

take :: Int -> Stream a -> [a]
take 0 _ = []
take n (Cons x xs) = x : take (n-1) xs
