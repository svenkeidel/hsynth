module Music.Interval where

import           Data.Ratio

-- A Just interval is a ratio between two frequencies
newtype Interval = Interval Rational

instance Show Interval where
  show (Interval i) = concat [show (numerator i), ":", show (denominator i)]

type Cent = Double

cents :: Interval -> Cent
cents (Interval rat) = 1200 * logBase 2 (fromRational rat)

ratio :: Interval -> Rational
ratio (Interval rat) = rat
