{-# LANGUAGE TemplateHaskell #-}
module OscillatorsSpec(main, spec) where

import           Control.Monad

import           Language.Frontend

import           Sound.Sine
import           Sound.Types

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Sine Oscillator" $ do
    it "run (sinA 1 100) ~ [sin (2*pi*x/100)|x<-[0..]]" $
      similar 100 1e-6 (run $(compile (sinA 1 100)))
                       [sin (2 * pi * x / 100) | x <- [0..]]

  -- describe "Saw Oscillator" $ do
  --   it "run (saw 1 100) ~ [mod (2*pi*x/100)|x<-[0..]]" $
  --     similar 100 1e-6 (run $(compile (sinA 1 100)))
  --                      [mod (2 * pi * x / 100) | x <- [0..]]

  where
    run :: AudioProcess a -> [Double]
    run (s,f) = let (a,s') = f ((),s)
                in a : run (s',f)

    similar :: Int -> Double -> [Double] -> [Double] -> Expectation
    similar n delta l1 l2 =
      -- expectationFailure $ printf "oscillator %s\nfunction   %s\ndelta %f"
      --                        (show (take n l1)) (show (take n l2)) delta
      forM_ (take n (zipWith (-) l1 l2)) $ \x ->
        abs x `shouldSatisfy` (Prelude.<= delta)
