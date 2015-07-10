{-# LANGUAGE Arrows #-}
module Sound.Compressor where

import           Control.Arrow
import           Sound.Types
import           Data.Stream (Stream)
import qualified Data.Stream as S

type Threshold = Double
type Ratio = Rational
type KneeWidth = Double
type Gain = Double
type Level = Double
type GainComputer = Gain -> Gain
type BranchDetector = Stream Level -> Stream Level
type Attack = Double
type Release = Double

compressor :: GainComputer -> BranchDetector -> Audio -> Audio
compressor gc bd = proc xs -> do
  ys <- fmap level2amp . fmap negate . bd . fmap negate . fmap gc . fmap amp2level -< xs
  returnA -< xs * ys

compressorLin :: GainComputer -> BranchDetector -> Audio -> Audio
compressorLin gc bd = proc xs -> do
  ys <- fmap level2amp . fmap gc . fmap amp2level . bd . fmap abs -< xs
  returnA -< xs * ys

amp2level :: Amplitude -> Level
amp2level a = 20 * logBase 10 (max (abs a) (1e-6))

level2amp :: Level -> Gain
level2amp x = 10 ** (x/20)

hardKnee :: Threshold -> Ratio -> GainComputer
hardKnee t r x =
  let slope = 1 / fromRational r - 1
      overshoot = x - t
  in max overshoot 0 * slope

softKnee :: Threshold -> Ratio -> KneeWidth -> GainComputer
softKnee t r w x
  | 2 * (x - t) < -w     = x
  | 2 * abs (x - t) <= w = x + (1/r' - 1) * (x - t + w / 2) ^^ (2::Int) / (2*w)
  | otherwise            = t + (x - t) / r'
  where
    r' = fromRational r

branching :: Attack -> Release -> Rate -> BranchDetector
branching attack release rate = S.tail . S.scan go 0
  where
    go yn1 x =
      if x > yn1
        then alphaAttack  * yn1 + (1-alphaAttack) * x
        else alphaRelease * yn1
    alphaAttack  = alpha attack rate
    alphaRelease = alpha release rate


branchingSmooth :: Attack -> Release -> Rate -> BranchDetector
branchingSmooth attack release rate = S.tail . S.scan go 0
  where
    go yn1 x =
      if x > yn1
        then alphaAttack  * yn1 + (1-alphaAttack)  * x
        else alphaRelease * yn1 + (1-alphaRelease) * x
    alphaAttack  = alpha attack rate
    alphaRelease = alpha release rate

decoupled :: Attack -> Release -> Rate -> BranchDetector
decoupled attack release rate = S.tail . fmap snd . S.scan go (0,0)
  where
    go (yn1,zn1) x =
      let yn = if x > yn1
                then x
                else alphaRelease * yn1
          zn = alphaAttack * zn1 + (1-alphaAttack) * yn
      in (yn,zn)
    alphaAttack  = alpha attack rate
    alphaRelease = alpha release rate

decoupledSmooth :: Attack -> Release -> Rate -> BranchDetector
decoupledSmooth attack release rate = S.tail . fmap snd . S.scan go (0,0)
  where
    go (yn1,zn1) x =
      let yn = if x > yn1
                then x
                else alphaRelease * yn1 + (1-alphaRelease) * x
          zn = alphaAttack * zn1 + (1-alphaAttack) * yn
      in (yn,zn)
    alphaAttack  = alpha attack rate
    alphaRelease = alpha release rate

alpha :: Double -> Rate -> Double
alpha x r
  | x > 0     = exp (-1 / (x * fromIntegral r))
  | otherwise = 0

