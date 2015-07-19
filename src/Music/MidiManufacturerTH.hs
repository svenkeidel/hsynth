module Music.MidiManufacturerTH where

import           Data.Char (ord,isAlphaNum,isSpace,toUpper)
import           Data.Csv
import           Data.Word
import           Data.Maybe (mapMaybe)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.ByteString.Char8 (unpack)
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Numeric (readHex)

import           Language.Haskell.TH.Syntax

data ManufacturerId
  = OneByte Word8
  | ThreeByte Word8 Word8 Word8
  deriving Show

newtype ManufacturerName = ManufacturerName String
  deriving Show
data ManufacturerEntry = ManufacturerEntry ManufacturerId ManufacturerName
  deriving Show

instance FromRecord ManufacturerEntry where
  parseRecord v = ManufacturerEntry <$> v .! 0 <*> v .! 1

instance FromField ManufacturerId where
  parseField f =
    let ws = words (unpack f)
    in case map (fst . head . readHex) ws of
      [a1]       -> return $ OneByte a1
      [a1,a2,a3] -> return $ ThreeByte a1 a2 a3
      _          -> fail "Manufacturer's id have either 1 or 3 bytes"

instance FromField ManufacturerName where
  parseField f = ManufacturerName <$> (simplifyManufacturerName <$> parseField f)

manufacturerFromCsv :: FilePath -> IO [ManufacturerEntry]
manufacturerFromCsv fp = do
  bs <- BS.readFile fp
  case decodeManufacturer bs of
    Left str -> error str
    Right ms -> return $ V.toList ms

decodeManufacturer :: ByteString -> Either String (Vector ManufacturerEntry)
decodeManufacturer = decodeWith (DecodeOptions { decDelimiter = fromIntegral (ord '|') }) NoHeader

-- | This function derives the following Haskell Code:
--
-- @
-- data Manufacturer
--  = KawaiMusicInstrumentsMFGCOLtd
--  | RolandCorporation
--  | ...
--
-- getManufacturer1Byte :: Get Manufacturer
-- getManufacturer1Byte = do
--  word1 <- getWord8
--  case word1 of
--    64 -> return KawaiMusicInstrumentsMFGCOLtd
--    65 -> return RolandCorporation
--    ...
--    b -> getManufacturer3Byte word1
--
-- getManufacturer3Byte :: Word8 -> Get Manufacturer
-- getManufacturer3Byte word1 = do
--   word2 <- getWord8
--   word3 <- getWord8
--   case (word1,word2,word3) of
--    (0,0,116) -> TaHorngMusicalInstrument
--    (0,0,117) -> ETekLabs
--    ...
--    _ -> error "unknown manufacturer"
-- @
deriveManufacturer :: [ManufacturerEntry] -> [Dec]
deriveManufacturer manufacturers = [manufacturerData,getManufacturer1ByteDec, getManufacturer3ByteDec]
  where
    manufacturerData = DataD [] (mkName "Manufacturer") [] (map constructor manufacturers) []

    constructor (ManufacturerEntry _ (ManufacturerName name)) =
      NormalC (mkName name) []

    getManufacturer1ByteDec =
      FunD (mkName "getManufacturer1Byte") [Clause [] (NormalB getManufacturer1ByteBody) []]

    getManufacturer1ByteBody =
      let word1 = mkName "word1"
      in DoE [ BindS (VarP word1) (VarE (mkName "B.getWord8"))
             , NoBindS $ CaseE (VarE word1)
                       $ mapMaybe getManufacturer1ByteCase manufacturers
                       ++ [Match WildP (NormalB (AppE (VarE (mkName "getManufacturer3Byte")) (VarE word1))) []]
             ]

    getManufacturer1ByteCase (ManufacturerEntry (OneByte word) (ManufacturerName manufacturer)) =
      Just $ Match (wordL word)
                   (NormalB (AppE (VarE (mkName "return")) (ConE (mkName manufacturer))))
                   []
    getManufacturer1ByteCase (ManufacturerEntry (ThreeByte _ _ _) _) =
      Nothing

    getManufacturer3ByteDec =
      FunD (mkName "getManufacturer3Byte") [Clause [VarP (mkName "word1")] (NormalB getManufacturer3ByteBody) []]

    getManufacturer3ByteBody =
      let word1 = mkName "word1"
          word2 = mkName "word2"
          word3 = mkName "word3"
      in DoE [ BindS (VarP word2) (VarE (mkName "B.getWord8"))
             , BindS (VarP word3) (VarE (mkName "B.getWord8"))
             , NoBindS $ CaseE (TupE [VarE word1, VarE word2, VarE word3])
                       $ mapMaybe getManufacturer3ByteCase manufacturers
                       ++ [Match WildP (NormalB (AppE (VarE (mkName "error")) (LitE (StringL "unknown manufacturer")))) []]
             ]

    getManufacturer3ByteCase (ManufacturerEntry (ThreeByte word1 word2 word3) (ManufacturerName manufacturer)) = 
      Just $ Match (TupP [wordL word1, wordL word2, wordL word3])
                   (NormalB (AppE (VarE (mkName "return")) (ConE (mkName manufacturer))))
                   []
    getManufacturer3ByteCase (ManufacturerEntry (OneByte _) _) =
      Nothing


    wordL w = LitP (IntegerL (fromIntegral w))


simplifyManufacturerName :: String -> String
simplifyManufacturerName = camelCase . dropSpecialCharacters . dropSuffix
  where
    camelCase :: String -> String
    camelCase = concat . map (mapHead toUpper) . words

    mapHead :: (a -> a) -> [a] -> [a]
    mapHead f (a:as) = f a : as
    mapHead _ [] = []

    dropSuffix :: String -> String
    dropSuffix = takeWhile (\c -> c /= ',' && c /= '(')

    dropSpecialCharacters :: String -> String
    dropSpecialCharacters = map (\c -> if isAlphaNum c || isSpace c then c else ' ')
