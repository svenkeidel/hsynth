module Data.Stream
  ( Stream(..)
  , head
  , tail
  , map
  , mapMaybe
  , switch
  , scan
  , unfold
  , unfold'
  , iterate
  , repeat
  , zip
  , zipWith
  , take
  , splitAt
  , (!!)
  , (<:>)
  ) where

import Prelude hiding ((!!),iterate,zipWith,zipWith3,head,tail,repeat,take,splitAt,map,zip)

-- Stream a isomorphic to Cofree Identity a
data Stream a = Cons !a (Stream a)

(<:>) :: a -> Stream a -> Stream a
(<:>) = Cons
infixr 5 <:>
{-# INLINE (<:>) #-}

head :: Stream a -> a
head (Cons a _) = a

tail :: Stream a -> Stream a
tail (Cons _ as) = as

map :: (a -> b) -> Stream a -> Stream b
map f (Cons x xs) = Cons (f x) (map f xs)
{-# NOINLINE map #-}

mapMaybe :: (a -> Maybe b) -> Stream a -> Stream b
mapMaybe f (Cons a as) =
  case f a of
    Just b  -> Cons b (mapMaybe f as)
    Nothing -> mapMaybe f as
{-# NOINLINE mapMaybe #-}

instance Functor Stream where
  fmap = map
  {-# INLINE fmap #-}

instance Num n => Num (Stream n) where
  (+) = zipWith (+)
  {-# INLINE (+) #-}
  (*) = zipWith (*)
  {-# INLINE (*) #-}
  (-) = zipWith (-)
  {-# INLINE (-) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = repeat . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional n => Fractional (Stream n) where
  (/) = zipWith (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational = repeat . fromRational
  {-# INLINE fromRational #-}

instance Floating n => Floating (Stream n) where
  pi = repeat pi
  {-# INLINE pi #-}
  exp = fmap exp
  {-# INLINE exp #-}
  sqrt = fmap sqrt
  {-# INLINE sqrt #-}
  log = fmap log
  {-# INLINE log #-}
  (**) = zipWith (**)
  {-# INLINE (**) #-}
  logBase = zipWith logBase
  {-# INLINE logBase #-}
  sin = fmap sin
  {-# INLINE sin #-}
  tan = fmap tan
  {-# INLINE tan #-}
  cos = fmap cos
  {-# INLINE cos #-}
  asin = fmap asin
  {-# INLINE asin #-}
  atan = fmap atan
  {-# INLINE atan #-}
  acos = fmap acos
  {-# INLINE acos #-}
  sinh = fmap sinh
  {-# INLINE sinh #-}
  tanh = fmap tanh
  {-# INLINE tanh #-}
  cosh = fmap cosh
  {-# INLINE cosh #-}
  asinh = fmap asinh
  {-# INLINE asinh #-}
  atanh = fmap atanh
  {-# INLINE atanh #-}
  acosh = fmap acosh
  {-# INLINE acosh #-}

switch :: Int -> Stream a -> Stream a -> Stream a
switch 0 _           ys = ys
switch n (Cons x xs) ys = Cons x (switch (n-1) xs ys)

scan :: (b -> a -> b) -> b -> Stream a -> Stream b
scan f b (Cons a as) = Cons b (scan f (f b a) as)
{-# NOINLINE scan #-}

unfold' :: (s,((),s) -> (a,s)) -> Stream a
unfold' (s,f) = unfold (\s' -> f ((),s')) s

unfold :: (s -> (a,s)) -> s -> Stream a
unfold f s0 =
  let (a,s) = f s0
  in Cons a (unfold f s)
{-# NOINLINE unfold #-}

iterate :: (a -> a) -> a -> Stream a
iterate f = unfold (\a -> (a,f a))
{-# INLINE iterate #-}

repeat :: a -> Stream a
repeat = unfold (\b -> (b,b))
{-# INLINE repeat #-}

zip :: Stream a -> Stream b -> Stream (a,b)
zip (Cons a as) (Cons b bs) = Cons (a,b) (zip as bs)
{-# NOINLINE zip #-}

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f as bs = fmap (uncurry f) (zip as bs)
{-# INLINE zipWith #-}

(!!) :: Stream a -> Int -> a
Cons a _ !! 0 = a
Cons _ s !! n = s !! (n-1)
infixl 9 !!

take :: Int -> Stream a -> [a]
take 0 _           = []
take n (Cons x xs) = x : take (n-1) xs

splitAt :: Int -> Stream a -> ([a], Stream a)
splitAt 0 xs          = ([],xs)
splitAt n (Cons x xs) =
  let (l,ys) = splitAt (n-1) xs
  in (x:l,ys)
