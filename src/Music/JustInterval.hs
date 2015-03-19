{-# LANGUAGE CPP #-}
module Music.JustInterval where

import Data.Ratio ((%))

-- A Just interval is a small ratio between two frequencies
newtype Interval = Interval Rational

type Cent = Double

cents :: Interval -> Cent
cents (Interval rat) = 1200 * logBase 2 (fromRational rat)

ratio :: Interval -> Rational
ratio (Interval rat) = rat

perfectUnison :: Interval
perfectUnison = Interval (1%1)
{-# INLINE perfectUnison #-}

perfectOctave :: Interval
perfectOctave = Interval (2%1)
{-# INLINE perfectOctave #-}

perfectFifth :: Interval
perfectFifth = Interval (3%2)
{-# INLINE perfectFifth #-}

perfectFourth :: Interval
perfectFourth = Interval (4%3)
{-# INLINE perfectFourth #-}

majorSecond :: Interval
majorSecond = Interval (9%8)
{-# INLINE majorSecond #-}

majorThird :: Interval
majorThird = Interval (5%4)
{-# INLINE majorThird #-}

majorSixth :: Interval
majorSixth = Interval (5%3)
{-# INLINE majorSixth #-}

majorSeventh :: Interval
majorSeventh = Interval (15%8)
{-# INLINE majorSeventh #-}

minorSecond :: Interval
minorSecond = Interval (16%15)
{-# INLINE minorSecond #-}

minorThird :: Interval
minorThird = Interval (6%5)
{-# INLINE minorThird #-}

minorSixth :: Interval
minorSixth = Interval (8%5)
{-# INLINE minorSixth #-}

smallMinorSeventh :: Interval
smallMinorSeventh = Interval (16%9)
{-# INLINE smallMinorSeventh #-}

largeMinorSeventh :: Interval
largeMinorSeventh = Interval (9%5)
{-# INLINE largeMinorSeventh #-}
