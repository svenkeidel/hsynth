module Data.Stream
  ( Stream(..)
  , map
  , mapMaybe
  , switch
  , scan
  , unfold
  , iterate
  , repeat
  , zip
  , zipWith
  , head
  , tail
  , take
  , splitAt
  , (!!)
  , (<:>)
  ) where

import Prelude hiding ((!!),iterate,zipWith,zipWith3,head,tail,repeat,take,splitAt,map,zip)
import Control.Arrow

data Stream a = Cons !a (Stream a)

(<:>) :: a -> Stream a -> Stream a
(<:>) = Cons
infixr 5 <:>
{-# INLINE (<:>) #-}

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

scan :: (a -> b -> a) -> a -> Stream b -> Stream a
scan f a (Cons b bs) = Cons a (scan f (f a b) bs)
{-# NOINLINE scan #-}

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

head :: Stream a -> a
head (Cons a _) = a
{-# INLINE CONLIKE head #-}

tail :: Stream a -> Stream a
tail (Cons _ as) = as
{-# INLINE CONLIKE tail #-}

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

{-# RULES
"zip/unfold/elim"  forall f g z0 z1. zip (unfold f z0) (unfold g z1) = unfold (\(s0,s1) -> let (a,s0') = f s0; (b,s1') = g s1 in ((a,b),(s0',s1'))) (z0,z1)
"zip/unfold/left"  forall f g z0 z1 as. zip (unfold f z0) (zip (unfold g z1) as) = map (\((a,b),c) -> (a,(b,c))) (zip (zip (unfold f z0) (unfold g z1)) as)
"zip/unfold/right" forall f g z0 z1 as. zip (unfold f z0) (zip as (unfold g z1)) = map (\((b,c),a) -> (b,(a,c))) (zip (zip (unfold f z0) (unfold g z1)) as)
"zip/fmap/left"    forall f as bs. zip (map f as) bs = map (first f) (zip as bs)
"zip/fmap/right"   forall f as bs. zip as (map f bs) = map (second f) (zip as bs)
"map/map"          forall f g as. map f (map g as) = map (f . g) as
"map/id"           forall as. map id as = as
"scan/fmap"        forall f g as z. scan f z (map g as) = scan (\a b -> f a (g b)) z as
"scan/scan"        forall f g as z0 z1. scan f z0 (scan g z1 as) = map fst $ scan (\(a,b) c -> let gbc = g b c in (f a gbc,gbc)) (z0,z1) as
"scan/zip/left"    forall f z0 as bs. zip (scan f z0 as) bs = scan (\(s,_) (a,b) -> (f s a,b)) (z0,head bs) (zip as bs)
"scan/zip/right"   forall f z0 as bs. zip as (scan f z0 bs) = scan (\(_,s) (a,b) -> (a,f s b)) (head as,z0) (zip as bs)
  #-}
