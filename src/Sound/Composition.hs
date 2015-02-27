module Sound.Composition where

import           Data.Stream (Stream(..))
import qualified Data.Stream as S

import           Sound.Types

combine :: [(Audio,Time,Duration)] -> Rate -> Audio
combine samples rate = combine' plus (map cut samples)
  where
    cut :: (Audio,Time,Duration) -> ([Double],Int)
    cut (audio,time,duration) =
      let at = truncate (time * fromIntegral rate)
          sampleLength = truncate (duration * fromIntegral rate)
      in (S.take sampleLength audio,at)

    plus :: Mixer
    plus (a:as) (Cons b bs) = Cons (a+b) (plus as bs)
    plus []             bs  = bs

type Mixer = [Double] -> Audio -> Audio

combine' :: Mixer -> [([Double],Int)] -> Audio
combine' mix = go (S.repeat 0) 0
  where
    go out@(Cons o os) t ((sample,at):rest)
      | at == t   = go (mix sample out) t rest
      | otherwise = Cons o (go os (t+1) ((sample,at):rest) )
    go out _ [] = out
