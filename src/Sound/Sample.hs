module Sound.Sample where

import           Sound.Types

-- | A sample has an infinite audio part and an finite teardown sound
data Sample = Sample Audio (Audio -> [Double])
