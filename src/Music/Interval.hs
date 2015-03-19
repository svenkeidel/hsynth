module Music.Interval where

import           Sound.Types (Frequency)
import           Music.Pitch

-- 1200th part of an octave is 1 Cent
type Cent = Int

-- Interval between two frequencies measured in Cents
newtype Interval = Interval Cent

interval :: Frequency -> Frequency -> Interval
interval f1 f2 = Interval $ round $ 1200 * logBase 2 (f2 / f1)

{-instance Show Interval where-}
  {-show (Interval i) = -}

{-showLong :: Interval -> String-}
{-showLong (Interval i) = case i of-}
  {-0 -> "Perfect Unison"-}
  {-1 -> "Minor Second"-}
  {-2 -> "Major Second"-}
  {-3 -> "Minor Third"-}
  {-4 -> "Perfect Fourth"-}
  {-5 -> "Minor Fifth"-}
  {-6 -> "Perfect Fifth"-}

{-showShort :: Interval -> String-}

--unison :: Interval
--unison = Interval (1 % 1)
--{-# INLINE unison #-}
--
--second :: Interval
--second = Interval (
--{-# INLINE second #-}
--
--third :: Interval
--third = Interval 4
--{-# INLINE third #-}
--
--fourth :: Interval
--fourth = Interval 5
--{-# INLINE fourth #-}
--
--fifth :: Interval
--fifth = Interval 7
--{-# INLINE fifth #-}
--
--sixth :: Interval
--sixth = Interval 9
--{-# INLINE sixth #-}
--
--seventh :: Interval
--seventh = Interval 11
--{-# INLINE seventh #-}
--
--octave :: Interval
--octave = Interval (2 % 1)
--{-# INLINE octave #-}
--
--
--perfect :: Interval -> Interval
--perfect = id
--{-# INLINE perfect #-}
--
--major :: Interval -> Interval
--major = perfect
--{-# INLINE major #-}
--
--minor :: Interval -> Interval
--minor (Interval i) = Interval (i - 1)
--{-# INLINE minor #-}
--
--(<+) :: Pitch -> Interval -> Pitch
--Pitch p <+ Interval i = Pitch (p + i)
--{-# INLINE (<+) #-}
--
--(<~) :: Pitch -> Interval -> Pitch
--p <~ i = p <+ invert i
--{-# INLINE (<~) #-}
--
--invert :: Interval -> Interval
--invert (Interval i) = Interval (negate (mod (-i) 12))
--{-# INLINE invert #-}
--
---- Shorthands
--per1 :: Interval
--per1 = perfect unison
--{-# INLINE per1 #-}
--
--min2 :: Interval
--min2 = minor second
--{-# INLINE min2 #-}
--
--maj2 :: Interval
--maj2 = major second
--{-# INLINE maj2 #-}
--
--min3 :: Interval
--min3 = minor third
--{-# INLINE min3 #-}
--
--maj3 :: Interval
--maj3 = major third
--{-# INLINE maj3 #-}
--
--per4 :: Interval
--per4 = perfect fourth
--{-# INLINE per4 #-}
--
--per5 :: Interval
--per5 = perfect fifth
--{-# INLINE per5 #-}
--
--min6 :: Interval
--min6 = minor sixth
--{-# INLINE min6 #-}
--
--maj6 :: Interval
--maj6 = major sixth
--{-# INLINE maj6 #-}
--
--min7 :: Interval
--min7 = minor seventh
--{-# INLINE min7 #-}
--
--maj7 :: Interval
--maj7 = major seventh
--{-# INLINE maj7 #-}
