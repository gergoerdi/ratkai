module RatBC.Words (loadWords) where

import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Data.Function (on)
import Control.Monad
import Data.Word
import Data.Char
import Text.Printf (printf)
import Data.List (sortBy, groupBy)
import qualified Data.Map as M

splitInto len bs = BL.take len bs : splitInto len (BL.drop len bs)

loadWords :: BL.ByteString -> M.Map Word8 [String]
loadWords bs =
    M.fromList .
    map fromGroup .
    groupBy ((==) `on` fst) .
    sortBy (compare `on` fst) .
    map fromEntry .
    take numWords .
    splitInto (wordLen + 1) .
    BL.drop startAddr $
    bs
  where
    startAddr = 0x3400
    numWords = 508

    fromGroup is@((id, _):_) = (id, map snd is)

wordLen = 5

fromEntry entry = (id, s)
  where
    word = BL.take wordLen entry
    id = BL.index entry wordLen
    s = concatMap toChar . BL.unpack $ word

toChar :: Word8 -> String
toChar b | 0x41 <= b && b < 0x5B = pure $ chr $ fromIntegral b + 0x20
         | b == 0x6C = "é"
         | b == 0x7a = "ö"
         | b == 0x40 = "ü"
         | b == 0x6d = "á"
         | b == 0x1b = "ő"
         | b == 0x61 = "í"
         | b == 0x76 = "ó"
         | b == 0x74 = "ú"
         | b == 0x1d = "ű"
         | 0xc1 <= b && b < 0xe0 = pure $ chr $ fromIntegral b - 0x80
         | 0x20 <= b && b < 0x40 = pure $ chr $ fromIntegral b
         | otherwise = printf "|0x%02x|" b
