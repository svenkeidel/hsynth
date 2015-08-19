module Main where

import           Flute
import qualified Sound.Driver.StdOut as Driver
import           System.Random
import           Options.Applicative

data FluteParams = FluteParams
  { duration :: Double
  , frequency :: Double
  , pressure :: Double
  , breath :: Double
  }

fluteParams :: Parser FluteParams
fluteParams = FluteParams
  <$> option auto
         (short 'd'
       <> long "duration"
       <> value 2.0
       <> showDefault
       <> help "the duration in seconds the flute is played")
  <*> option auto
         (short 'f'
       <> long "frequency"
       <> value 440
       <> showDefault
       <> help "the frequency which gives the base tone of the flute")
  <*> option auto
         (short 'p'
       <> long "pressure"
       <> value 0.9
       <> showDefault
       <> help "the air pressure that is used to play the flute")
  <*> option auto
         (short 'b'
       <> value 0.1
       <> showDefault
       <> long "breath")

main = do
  params <- execParser $ info fluteParams fullDesc
  g <- newStdGen
  Driver.runAudio $ flute (duration params)
                          0.5
                          (frequency params)
                          (pressure params)
                          (breath params)
                          g
                          48000
