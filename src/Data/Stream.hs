module Data.Stream where

import Prelude hiding ((!!),iterate,zipWith,zipWith3,head,tail,repeat,take,map)
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
{-# NOINLINE unfold #-}

iterate :: (a -> a) -> a -> Stream a
iterate f a = Cons a (iterate f (f a))
{-# NOINLINE iterate #-}

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

head :: Stream a -> a
head (Cons a _) = a
{-# INLINE CONLIKE head #-}

tail :: Stream a -> Stream a
tail (Cons _ as) = as
{-# INLINE CONLIKE tail #-}

repeat :: a -> Stream a
repeat a = Cons a (repeat a)
{-# NOINLINE repeat #-}

(!!) :: Stream a -> Int -> a
Cons a _ !! 0 = a
Cons _ s !! n = s !! (n-1)
infixl 9 !!

take :: Int -> Stream a -> [a]
take 0 _           = []
take n (Cons x xs) = x : take (n-1) xs

-- This law does not hold !!!!
--"fmap/scan"        forall f g as z. map f (scan g z as) = scan (\a b -> f (g a b)) z as

{-# RULES
"scan/fmap"        forall f g as z. scan f z (map g as) = scan (\a b -> f a (g b)) z as
"iterate/fmap"     forall f g z. map f (iterate g z) = iterate (f . g) (f z)
"repeat/fmap"      forall f a. map f (repeat a) = repeat (f a)
"unfold/fmap"      forall f g z. map f (unfold g z) = unfold (g >>> first f) z
"unfold/scan"      forall f g s0 b0. scan f b0 (unfold g s0) =
                                      let go (s,b) =
                                            let (a,s') = g s
                                                b'     = f a b
                                            in (b',(s',b'))
                                      in unfold go (s0,b0)

"zipWith2/fmap1"   forall f g as bs. zipWith f (map g as) bs = zipWith (\a b -> f (g a) b) as bs
"zipWith2/fmap2"   forall f g as bs. zipWith f as (map g bs) = zipWith (\a b -> f a (g b)) as bs

"zipWith3/fmap1"   forall f g as bs cs. zipWith3 f (map g as) bs cs = zipWith3 (\a b c -> f (g a) b c) as bs cs
"zipWith3/fmap2"   forall f g as bs cs. zipWith3 f as (map g bs) cs = zipWith3 (\a b c -> f a (g b) c) as bs cs
"zipWith3/fmap3"   forall f g as bs cs. zipWith3 f as bs (map g cs) = zipWith3 (\a b c -> f a b (g c)) as bs cs

"zipWith4/fmap1"   forall f g as bs cs ds. zipWith4 f (map g as) bs cs ds = zipWith4 (\a b c d -> f (g a) b c d) as bs cs ds
"zipWith4/fmap2"   forall f g as bs cs ds. zipWith4 f as (map g bs) cs ds = zipWith4 (\a b c d -> f a (g b) c d) as bs cs ds
"zipWith4/fmap3"   forall f g as bs cs ds. zipWith4 f as bs (map g cs) ds = zipWith4 (\a b c d -> f a b (g c) d) as bs cs ds
"zipWith4/fmap4"   forall f g as bs cs ds. zipWith4 f as bs cs (map g ds) = zipWith4 (\a b c d -> f a b c (g d)) as bs cs ds

"zipWith5/fmap1"   forall f g as bs cs ds es. zipWith5 f (map g as) bs cs ds es = zipWith5 (\a b c d e -> f (g a) b c d e) as bs cs ds es
"zipWith5/fmap2"   forall f g as bs cs ds es. zipWith5 f as (map g bs) cs ds es = zipWith5 (\a b c d e -> f a (g b) c d e) as bs cs ds es
"zipWith5/fmap3"   forall f g as bs cs ds es. zipWith5 f as bs (map g cs) ds es = zipWith5 (\a b c d e -> f a b (g c) d e) as bs cs ds es
"zipWith5/fmap4"   forall f g as bs cs ds es. zipWith5 f as bs cs (map g ds) es = zipWith5 (\a b c d e -> f a b c (g d) e) as bs cs ds es
"zipWith5/fmap5"   forall f g as bs cs ds es. zipWith5 f as bs cs ds (map g es) = zipWith5 (\a b c d e -> f a b c d (g e)) as bs cs ds es

"zipWith6/fmap1"   forall h i as bs cs ds es fs. zipWith6 h (map i as) bs cs ds es fs = zipWith6 (\a b c d e f -> h (i a) b c d e f) as bs cs ds es fs
"zipWith6/fmap2"   forall h i as bs cs ds es fs. zipWith6 h as (map i bs) cs ds es fs = zipWith6 (\a b c d e f -> h a (i b) c d e f) as bs cs ds es fs
"zipWith6/fmap3"   forall h i as bs cs ds es fs. zipWith6 h as bs (map i cs) ds es fs = zipWith6 (\a b c d e f -> h a b (i c) d e f) as bs cs ds es fs
"zipWith6/fmap4"   forall h i as bs cs ds es fs. zipWith6 h as bs cs (map i ds) es fs = zipWith6 (\a b c d e f -> h a b c (i d) e f) as bs cs ds es fs
"zipWith6/fmap5"   forall h i as bs cs ds es fs. zipWith6 h as bs cs ds (map i es) fs = zipWith6 (\a b c d e f -> h a b c d (i e) f) as bs cs ds es fs
"zipWith6/fmap6"   forall h i as bs cs ds es fs. zipWith6 h as bs cs ds es (map i fs) = zipWith6 (\a b c d e f -> h a b c d e (i f)) as bs cs ds es fs

"zipWith7/fmap1"   forall h i as bs cs ds es fs gs. zipWith7 h (map i as) bs cs ds es fs gs = zipWith7 (\a b c d e f g -> h (i a) b c d e f g) as bs cs ds es fs gs
"zipWith7/fmap2"   forall h i as bs cs ds es fs gs. zipWith7 h as (map i bs) cs ds es fs gs = zipWith7 (\a b c d e f g -> h a (i b) c d e f g) as bs cs ds es fs gs
"zipWith7/fmap3"   forall h i as bs cs ds es fs gs. zipWith7 h as bs (map i cs) ds es fs gs = zipWith7 (\a b c d e f g -> h a b (i c) d e f g) as bs cs ds es fs gs
"zipWith7/fmap4"   forall h i as bs cs ds es fs gs. zipWith7 h as bs cs (map i ds) es fs gs = zipWith7 (\a b c d e f g -> h a b c (i d) e f g) as bs cs ds es fs gs
"zipWith7/fmap5"   forall h i as bs cs ds es fs gs. zipWith7 h as bs cs ds (map i es) fs gs = zipWith7 (\a b c d e f g -> h a b c d (i e) f g) as bs cs ds es fs gs
"zipWith7/fmap6"   forall h i as bs cs ds es fs gs. zipWith7 h as bs cs ds es (map i fs) gs = zipWith7 (\a b c d e f g -> h a b c d e (i f) g) as bs cs ds es fs gs
"zipWith7/fmap7"   forall h i as bs cs ds es fs gs. zipWith7 h as bs cs ds es fs (map i gs) = zipWith7 (\a b c d e f g -> h a b c d e f (i g)) as bs cs ds es fs gs

"fmap/zipWith2"    forall f g as bs. map f (zipWith g as bs) = zipWith (\a b -> f (g a b)) as bs
"fmap/zipWith3"    forall f g as bs cs. map f (zipWith3 g as bs cs) = zipWith3 (\a b c -> f (g a b c)) as bs cs
"fmap/zipWith4"    forall f g as bs cs ds. map f (zipWith4 g as bs cs ds) = zipWith4 (\a b c d -> f (g a b c d)) as bs cs ds
"fmap/zipWith5"    forall f g as bs cs ds es. map f (zipWith5 g as bs cs ds es) = zipWith5 (\a b c d e -> f (g a b c d e)) as bs cs ds es
"fmap/zipWith6"    forall h i as bs cs ds es fs. map h (zipWith6 i as bs cs ds es fs) = zipWith6 (\a b c d e f -> h (i a b c d e f)) as bs cs ds es fs
"fmap/zipWith7"    forall h i as bs cs ds es fs gs. map h (zipWith7 i as bs cs ds es fs gs) = zipWith7 (\a b c d e f g -> h (i a b c d e f g)) as bs cs ds es fs gs

"zipWith2/assoc1"  forall f g as bs cs. zipWith f (zipWith g as bs) cs = zipWith3 (\a b c -> f (g a b) c) as bs cs
"zipWith2/assoc2"  forall f g as bs cs. zipWith f as (zipWith g bs cs) = zipWith3 (\a b c -> f a (g b c)) as bs cs
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

"zipWith6/assoc1"  forall h i as bs cs ds es fs gs. zipWith6 h (zipWith i as bs) cs ds es fs gs = zipWith7 (\a b c d e f g -> h (i a b) c d e f g) as bs cs ds es fs gs
"zipWith6/assoc2"  forall h i as bs cs ds es fs gs. zipWith6 h as (zipWith i bs cs) ds es fs gs = zipWith7 (\a b c d e f g -> h a (i b c) d e f g) as bs cs ds es fs gs
"zipWith6/assoc3"  forall h i as bs cs ds es fs gs. zipWith6 h as bs (zipWith i cs ds) es fs gs = zipWith7 (\a b c d e f g -> h a b (i c d) e f g) as bs cs ds es fs gs
"zipWith6/assoc4"  forall h i as bs cs ds es fs gs. zipWith6 h as bs cs (zipWith i ds es) fs gs = zipWith7 (\a b c d e f g -> h a b c (i d e) f g) as bs cs ds es fs gs
"zipWith6/assoc5"  forall h i as bs cs ds es fs gs. zipWith6 h as bs cs ds (zipWith i es fs) gs = zipWith7 (\a b c d e f g -> h a b c d (i e f) g) as bs cs ds es fs gs
"zipWith6/assoc6"  forall h i as bs cs ds es fs gs. zipWith6 h as bs cs ds es (zipWith i fs gs) = zipWith7 (\a b c d e f g -> h a b c d e (i f g)) as bs cs ds es fs gs

"zipWith2/repeat1" forall f x as. zipWith f (repeat x) as = map (f x) as
"zipWith2/repeat2" forall f x as. zipWith f as (repeat x) = map (\a -> f a x) as

"zipWith3/repeat1" forall f x as bs. zipWith3 f (repeat x) as bs = zipWith (f x) as bs
"zipWith3/repeat2" forall f x as bs. zipWith3 f as (repeat x) bs = zipWith (\a -> f a x) as bs
"zipWith3/repeat3" forall f x as bs. zipWith3 f as bs (repeat x) = zipWith (\a b -> f a b x) as bs

"zipWith4/repeat1" forall f x as bs cs. zipWith4 f (repeat x) as bs cs = zipWith3 (f x) as bs cs
"zipWith4/repeat2" forall f x as bs cs. zipWith4 f as (repeat x) bs cs = zipWith3 (\a -> f a x) as bs cs
"zipWith4/repeat3" forall f x as bs cs. zipWith4 f as bs (repeat x) cs = zipWith3 (\a b -> f a b x) as bs cs
"zipWith4/repeat4" forall f x as bs cs. zipWith4 f as bs cs (repeat x) = zipWith3 (\a b c -> f a b c x) as bs cs

"zipWith5/repeat1" forall f x as bs cs ds. zipWith5 f (repeat x) as bs cs ds = zipWith4 (f x) as bs cs ds
"zipWith5/repeat2" forall f x as bs cs ds. zipWith5 f as (repeat x) bs cs ds = zipWith4 (\a -> f a x) as bs cs ds
"zipWith5/repeat3" forall f x as bs cs ds. zipWith5 f as bs (repeat x) cs ds = zipWith4 (\a b -> f a b x) as bs cs ds
"zipWith5/repeat4" forall f x as bs cs ds. zipWith5 f as bs cs (repeat x) ds = zipWith4 (\a b c -> f a b c x) as bs cs ds
"zipWith5/repeat5" forall f x as bs cs ds. zipWith5 f as bs cs ds (repeat x) = zipWith4 (\a b c d -> f a b c d x) as bs cs ds

"zipWith6/repeat1" forall f x as bs cs ds es. zipWith6 f (repeat x) as bs cs ds es = zipWith5 (f x) as bs cs ds es
"zipWith6/repeat2" forall f x as bs cs ds es. zipWith6 f as (repeat x) bs cs ds es = zipWith5 (\a -> f a x) as bs cs ds es
"zipWith6/repeat3" forall f x as bs cs ds es. zipWith6 f as bs (repeat x) cs ds es = zipWith5 (\a b -> f a b x) as bs cs ds es
"zipWith6/repeat4" forall f x as bs cs ds es. zipWith6 f as bs cs (repeat x) ds es = zipWith5 (\a b c -> f a b c x) as bs cs ds es
"zipWith6/repeat5" forall f x as bs cs ds es. zipWith6 f as bs cs ds (repeat x) es = zipWith5 (\a b c d -> f a b c d x) as bs cs ds es
"zipWith6/repeat6" forall f x as bs cs ds es. zipWith6 f as bs cs ds es (repeat x) = zipWith5 (\a b c d e -> f a b c d e x) as bs cs ds es

"zipWith7/repeat1" forall g x as bs cs ds es fs. zipWith7 g (repeat x) as bs cs ds es fs = zipWith6 (g x) as bs cs ds es fs
"zipWith7/repeat2" forall g x as bs cs ds es fs. zipWith7 g as (repeat x) bs cs ds es fs = zipWith6 (\a -> g a x) as bs cs ds es fs
"zipWith7/repeat3" forall g x as bs cs ds es fs. zipWith7 g as bs (repeat x) cs ds es fs = zipWith6 (\a b -> g a b x) as bs cs ds es fs
"zipWith7/repeat4" forall g x as bs cs ds es fs. zipWith7 g as bs cs (repeat x) ds es fs = zipWith6 (\a b c -> g a b c x) as bs cs ds es fs
"zipWith7/repeat5" forall g x as bs cs ds es fs. zipWith7 g as bs cs ds (repeat x) es fs = zipWith6 (\a b c d -> g a b c d x) as bs cs ds es fs
"zipWith7/repeat6" forall g x as bs cs ds es fs. zipWith7 g as bs cs ds es (repeat x) fs = zipWith6 (\a b c d e -> g a b c d e x) as bs cs ds es fs
"zipWith7/repeat7" forall g x as bs cs ds es fs. zipWith7 g as bs cs ds es fs (repeat x) = zipWith6 (\a b c d e f -> g a b c d e f x) as bs cs ds es fs
  #-}

-- "zipWith2/unroll1" forall f x xs as. zipWith f (Cons x xs) as = f x (head as) <:> zipWith f xs (tail as)
-- "zipWith2/unroll2" forall f x xs as. zipWith f as (Cons x xs) = f (head as) x <:> zipWith f (tail as) xs
--
-- "zipWith3/unroll1" forall f x xs as bs. zipWith3 f (Cons x xs) as bs = f x (head as) (head bs) <:> zipWith3 f xs (tail as) (tail bs)
-- "zipWith3/unroll2" forall f x xs as bs. zipWith3 f as (Cons x xs) bs = f (head as) x (head bs) <:> zipWith3 f (tail as) xs (tail bs)
-- "zipWith3/unroll3" forall f x xs as bs. zipWith3 f as bs (Cons x xs) = f (head as) (head bs) x <:> zipWith3 f (tail as) (tail bs) xs
-- 
-- "zipWith4/unroll1" forall f x xs as bs cs. zipWith4 f (Cons x xs) as bs cs = f x (head as) (head bs) (head cs) <:> zipWith4 f xs (tail as) (tail bs) (tail cs)
-- "zipWith4/unroll2" forall f x xs as bs cs. zipWith4 f as (Cons x xs) bs cs = f (head as) x (head bs) (head cs) <:> zipWith4 f (tail as) xs (tail bs) (tail cs)
-- "zipWith4/unroll3" forall f x xs as bs cs. zipWith4 f as bs (Cons x xs) cs = f (head as) (head bs) x (head cs) <:> zipWith4 f (tail as) (tail bs) xs (tail cs)
-- "zipWith4/unroll4" forall f x xs as bs cs. zipWith4 f as bs cs (Cons x xs) = f (head as) (head bs) (head cs) x <:> zipWith4 f (tail as) (tail bs) (tail cs) xs
