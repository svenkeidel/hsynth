{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
module Music.MidiManufacturer where

import           Data.Word (Word8)
import           Data.Binary (Get)
import qualified Data.Binary as B
import           Language.Haskell.TH (runIO)
import           Music.MidiManufacturerTH

$(runIO (deriveManufacturer <$> manufacturerFromCsv "data/manufacturer.csv"))
getManufacturer1Byte :: Get Manufacturer
getManufacturer3Byte :: Word8 -> Get Manufacturer

deriving instance Show Manufacturer
deriving instance Eq Manufacturer
