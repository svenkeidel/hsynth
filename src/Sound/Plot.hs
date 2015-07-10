module Sound.Plot where

import qualified Data.Stream as S

import           Numeric.LinearAlgebra

import           Sound.Types

import           Graphics.Rendering.Plot (Series)
import qualified Graphics.Rendering.Plot as P
import           Graphics.Rendering.Plot.Gtk (PlotHandle)
import qualified Graphics.Rendering.Plot.Gtk as P

plot :: Int -> Rate -> [Audio] -> IO PlotHandle
plot n r audio = P.display $ do
  P.plot (P.Line, map (fromList . S.take n) audio :: [Series])
  P.withTextDefaults $ P.setFontFamily "Ubuntu"
  P.xlabel "samples"
  P.ylabel "amplitude"
  P.yrange P.Linear (-1) 1
