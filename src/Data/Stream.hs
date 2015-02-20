module Data.Stream where

import Prelude hiding ((!!),iterate,zipWith,zipWith3,tail,repeat,take,map)

data Stream a = Cons !a (Stream a)

(<:>) :: a -> Stream a -> Stream a
(<:>) = Cons
infixr 5 <:>
{-# INLINE (<:>) #-}

map :: (a -> b) -> Stream a -> Stream b
map f (Cons x xs) = Cons (f x) (map f xs)
{-# NOINLINE map #-}

instance Functor Stream where
  fmap = map

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
{-# NOINLINE scan #-}

unfold :: (s -> (a,s)) -> s -> Stream a
unfold f s0 =
  let (a,s) = f s0
  in Cons a (unfold f s)

iterate :: (a -> a) -> a -> Stream a
iterate f a = Cons a (iterate f (f a))

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipWith f as bs)
{-# NOINLINE zipWith #-}

zipWith3 :: (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
zipWith3 f (Cons a as) (Cons b bs) (Cons c cs) = Cons (f a b c) (zipWith3 f as bs cs)
{-# NOINLINE zipWith3 #-}

zipWith4 :: (a -> b -> c -> d -> e) -> Stream a -> Stream b -> Stream c -> Stream d -> Stream e
zipWith4 f (Cons a as) (Cons b bs) (Cons c cs) (Cons d ds) = Cons (f a b c d) (zipWith4 f as bs cs ds)
{-# NOINLINE zipWith4 #-}

zipWith5 :: (a -> b -> c -> d -> e -> f) -> Stream a -> Stream b -> Stream c -> Stream d -> Stream e -> Stream f
zipWith5 f (Cons a as) (Cons b bs) (Cons c cs) (Cons d ds) (Cons e es) = Cons (f a b c d e) (zipWith5 f as bs cs ds es)
{-# NOINLINE zipWith5 #-}

zipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> Stream a -> Stream b -> Stream c -> Stream d -> Stream e -> Stream f -> Stream g
zipWith6 g (Cons a as) (Cons b bs) (Cons c cs) (Cons d ds) (Cons e es) (Cons f fs) = Cons (g a b c d e f) (zipWith6 g as bs cs ds es fs)
{-# NOINLINE zipWith6 #-}

zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> Stream a -> Stream b -> Stream c -> Stream d -> Stream e -> Stream f -> Stream g -> Stream h
zipWith7 h (Cons a as) (Cons b bs) (Cons c cs) (Cons d ds) (Cons e es) (Cons f fs) (Cons g gs) = Cons (h a b c d e f g) (zipWith7 h as bs cs ds es fs gs)
{-# NOINLINE zipWith7 #-}

{-# RULES
"scan/fmap"        forall f g as z. scan f z (map g as) = scan (\a b -> f a (g b)) z as
"fmap/scan"        forall f g as z. map f (scan g z as) = scan (\a b -> f (g a b)) z as

"zipWith2/assoc1"  forall f g as bs cs. zipWith f as (zipWith g bs cs) = zipWith3 (\a b c -> f a (g b c)) as bs cs
"zipWith2/assoc2"  forall f g as bs cs. zipWith f (zipWith g as bs) cs = zipWith3 (\a b c -> f (g a b) c) as bs cs
"zipWith2/assoc3"  forall f g as bs cs ds. zipWith f (zipWith3 g as bs cs) ds = zipWith4 (\a b c d -> f (g a b c) d) as bs cs ds
"zipWith2/assoc4"  forall f g as bs cs ds. zipWith f as (zipWith3 g bs cs ds) = zipWith4 (\a b c d -> f a (g b c d)) as bs cs ds
"zipWith2/assoc5"  forall f g as bs cs ds es. zipWith f (zipWith4 g as bs cs ds) es = zipWith5 (\a b c d e -> f (g a b c d) e) as bs cs ds es
"zipWith2/assoc6"  forall f g as bs cs ds es. zipWith f as (zipWith4 g bs cs ds es) = zipWith5 (\a b c d e -> f a (g b c d e)) as bs cs ds es
"zipWith2/assoc7"  forall h i as bs cs ds es fs. zipWith h (zipWith5 i as bs cs ds es) fs = zipWith6 (\a b c d e f -> h (i a b c d e) f) as bs cs ds es fs
"zipWith2/assoc8"  forall h i as bs cs ds es fs. zipWith h as (zipWith5 i bs cs ds es fs) = zipWith6 (\a b c d e f -> h a (i b c d e f)) as bs cs ds es fs
"zipWith2/assoc9"  forall h i as bs cs ds es fs gs. zipWith h (zipWith6 i as bs cs ds es fs) gs = zipWith7 (\a b c d e f g -> h (i a b c d e f) g) as bs cs ds es fs gs
"zipWith2/assoc10" forall h i as bs cs ds es fs gs. zipWith h as (zipWith6 i bs cs ds es fs gs) = zipWith7 (\a b c d e f g -> h a (i b c d e f g)) as bs cs ds es fs gs

"zipWith3/assoc1"  forall f g as bs cs ds. zipWith3 f (zipWith g as bs) cs ds = zipWith4 (\a b c d -> f (g a b) c d) as bs cs ds
"zipWith3/assoc2"  forall f g as bs cs ds. zipWith3 f as (zipWith g bs cs) ds = zipWith4 (\a b c d -> f a (g b c) d) as bs cs ds
"zipWith3/assoc3"  forall f g as bs cs ds. zipWith3 f as bs (zipWith g cs ds) = zipWith4 (\a b c d -> f a b (g c d)) as bs cs ds
"zipWith3/assoc4"  forall f g as bs cs ds es. zipWith3 f (zipWith3 g as bs cs) ds es = zipWith5 (\a b c d e -> f (g a b c) d e) as bs cs ds es
"zipWith3/assoc5"  forall f g as bs cs ds es. zipWith3 f as (zipWith3 g bs cs ds) es = zipWith5 (\a b c d e -> f a (g b c d) e) as bs cs ds es
"zipWith3/assoc6"  forall f g as bs cs ds es. zipWith3 f as bs (zipWith3 g cs ds es) = zipWith5 (\a b c d e -> f a b (g c d e)) as bs cs ds es
"zipWith3/assoc7"  forall h i as bs cs ds es fs. zipWith3 h (zipWith4 i as bs cs ds) es fs = zipWith6 (\a b c d e f -> h (i a b c d) e f) as bs cs ds es fs
"zipWith3/assoc8"  forall h i as bs cs ds es fs. zipWith3 h as (zipWith4 i bs cs ds es) fs = zipWith6 (\a b c d e f -> h a (i b c d e) f) as bs cs ds es fs
"zipWith3/assoc9"  forall h i as bs cs ds es fs. zipWith3 h as bs (zipWith4 i cs ds es fs) = zipWith6 (\a b c d e f -> h a b (i c d e f)) as bs cs ds es fs
"zipWith3/assoc10" forall h i as bs cs ds es fs gs. zipWith3 h (zipWith5 i as bs cs ds es) fs gs = zipWith7 (\a b c d e f g -> h (i a b c d e) f g) as bs cs ds es fs gs
"zipWith3/assoc11" forall h i as bs cs ds es fs gs. zipWith3 h as (zipWith5 i bs cs ds es fs) gs = zipWith7 (\a b c d e f g -> h a (i b c d e f) g) as bs cs ds es fs gs
"zipWith3/assoc12" forall h i as bs cs ds es fs gs. zipWith3 h as bs (zipWith5 i cs ds es fs gs) = zipWith7 (\a b c d e f g -> h a b (i c d e f g)) as bs cs ds es fs gs

"zipWith4/assoc1"  forall h i as bs cs ds es. zipWith4 h (zipWith i as bs) cs ds es = zipWith5 (\a b c d e -> h (i a b) c d e) as bs cs ds es
"zipWith4/assoc2"  forall h i as bs cs ds es. zipWith4 h as (zipWith i bs cs) ds es = zipWith5 (\a b c d e -> h a (i b c) d e) as bs cs ds es
"zipWith4/assoc3"  forall h i as bs cs ds es. zipWith4 h as bs (zipWith i cs ds) es = zipWith5 (\a b c d e -> h a b (i c d) e) as bs cs ds es
"zipWith4/assoc4"  forall h i as bs cs ds es. zipWith4 h as bs cs (zipWith i ds es) = zipWith5 (\a b c d e -> h a b c (i d e)) as bs cs ds es
"zipWith4/assoc5"  forall h i as bs cs ds es fs. zipWith4 h (zipWith3 i as bs cs) ds es fs = zipWith6 (\a b c d e f -> h (i a b c) d e f) as bs cs ds es fs
"zipWith4/assoc6"  forall h i as bs cs ds es fs. zipWith4 h as (zipWith3 i bs cs ds) es fs = zipWith6 (\a b c d e f -> h a (i b c d) e f) as bs cs ds es fs
"zipWith4/assoc7"  forall h i as bs cs ds es fs. zipWith4 h as bs (zipWith3 i cs ds es) fs = zipWith6 (\a b c d e f -> h a b (i c d e) f) as bs cs ds es fs
"zipWith4/assoc8"  forall h i as bs cs ds es fs. zipWith4 h as bs cs (zipWith3 i ds es fs) = zipWith6 (\a b c d e f -> h a b c (i d e f)) as bs cs ds es fs
"zipWith4/assoc9"  forall h i as bs cs ds es fs gs. zipWith4 h (zipWith4 i as bs cs ds) es fs gs = zipWith7 (\a b c d e f g -> h (i a b c d) e f g) as bs cs ds es fs gs
"zipWith4/assoc10" forall h i as bs cs ds es fs gs. zipWith4 h as (zipWith4 i bs cs ds es) fs gs = zipWith7 (\a b c d e f g -> h a (i b c d e) f g) as bs cs ds es fs gs
"zipWith4/assoc11" forall h i as bs cs ds es fs gs. zipWith4 h as bs (zipWith4 i cs ds es fs) gs = zipWith7 (\a b c d e f g -> h a b (i c d e f) g) as bs cs ds es fs gs
"zipWith4/assoc11" forall h i as bs cs ds es fs gs. zipWith4 h as bs cs (zipWith4 i ds es fs gs) = zipWith7 (\a b c d e f g -> h a b c (i d e f g)) as bs cs ds es fs gs

"zipWith5/assoc1"  forall h i as bs cs ds es fs. zipWith5 h (zipWith i as bs) cs ds es fs = zipWith6 (\a b c d e f -> h (i a b) c d e f) as bs cs ds es fs
"zipWith5/assoc2"  forall h i as bs cs ds es fs. zipWith5 h as (zipWith i bs cs) ds es fs = zipWith6 (\a b c d e f -> h a (i b c) d e f) as bs cs ds es fs
"zipWith5/assoc3"  forall h i as bs cs ds es fs. zipWith5 h as bs (zipWith i cs ds) es fs = zipWith6 (\a b c d e f -> h a b (i c d) e f) as bs cs ds es fs
"zipWith5/assoc4"  forall h i as bs cs ds es fs. zipWith5 h as bs cs (zipWith i ds es) fs = zipWith6 (\a b c d e f -> h a b c (i d e) f) as bs cs ds es fs
"zipWith5/assoc5"  forall h i as bs cs ds es fs. zipWith5 h as bs cs ds (zipWith i es fs) = zipWith6 (\a b c d e f -> h a b c d (i e f)) as bs cs ds es fs
"zipWith5/assoc6"  forall h i as bs cs ds es fs gs. zipWith5 h (zipWith3 i as bs cs) ds es fs gs = zipWith7 (\a b c d e f g -> h (i a b c) d e f g) as bs cs ds es fs gs
"zipWith5/assoc7"  forall h i as bs cs ds es fs gs. zipWith5 h as (zipWith3 i bs cs ds) es fs gs = zipWith7 (\a b c d e f g -> h a (i b c d) e f g) as bs cs ds es fs gs
"zipWith5/assoc8"  forall h i as bs cs ds es fs gs. zipWith5 h as bs (zipWith3 i cs ds es) fs gs = zipWith7 (\a b c d e f g -> h a b (i c d e) f g) as bs cs ds es fs gs
"zipWith5/assoc9"  forall h i as bs cs ds es fs gs. zipWith5 h as bs cs (zipWith3 i ds es fs) gs = zipWith7 (\a b c d e f g -> h a b c (i d e f) g) as bs cs ds es fs gs
"zipWith5/assoc10" forall h i as bs cs ds es fs gs. zipWith5 h as bs cs ds (zipWith3 i es fs gs) = zipWith7 (\a b c d e f g -> h a b c d (i e f g)) as bs cs ds es fs gs
  #-}

tail :: Stream a -> Stream a
tail (Cons _ as) = as
{-# INLINE tail #-}

repeat :: a -> Stream a
repeat a = Cons a (repeat a)

(!!) :: Stream a -> Int -> a
Cons a _ !! 0 = a
Cons _ s !! n = s !! (n-1)
infixl 9 !!

take :: Int -> Stream a -> [a]
take 0 _           = []
take n (Cons x xs) = x : take (n-1) xs
