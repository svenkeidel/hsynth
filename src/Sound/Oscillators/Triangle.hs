module Sound.Oscillators.Triangle where

import Prelude ()
import Language.Frontend
import Sound.Oscillators.Saw
import Sound.Types

triangle :: Frequency -> Rate -> Signal () Double
triangle freq rate = saw freq rate >>> arr' (\s -> 2 * abs s - 1)

