{-# LANGUAGE TupleSections, OverloadedStrings #-}
module RatBC.Text (loadMessages) where

import qualified Data.ByteString.Lazy as BL
import Data.Char
import Text.Printf (printf)
import Data.Word
import Data.List (isPrefixOf)

loadMessages :: BL.ByteString -> ([String], [String])
loadMessages bs = (f bank1 num1, f bank2 num2)
  where
    f startAddr numMsgs =
        takeWhile (not . ("NTR" `isPrefixOf`)) .
        map (concatMap toChar . BL.unpack) .
        take numMsgs .
        BL.split 0xFF .
        BL.drop startAddr $
        bs

    bank1 = 0x6000
    num1 = 0xdb

    bank2 = 0x8000
    num2 = 0xd9

toChar :: Word8 -> String
toChar b | b >= 128 = toChar (b - 128) ++ " "
         | b == 0x00 = "ö"
         | b == 0x1b = "Ő"
         | b == 0x2a = "ü"
         | b == 0x40 = "Ü"
         | b == 0x5f = "ű"
         | b == 0x61 = "Í"
         | b == 0x62 = "í"
         | b == 0x64 = "ő"
         | b == 0x6c = "É"
         | b == 0x6d = "Á"
         | b == 0x70 = "á"
         | b == 0x71 = "é"
         | b == 0x75 = "Ú"
         | b == 0x78 = "ú"
         | b == 0x79 = "ó"
         | b == 0x7a = "Ö"
         | 0x01 <= b && b < 0x1B = pure $ chr $ fromIntegral b + 0x60
         | 0x20 <= b && b < 0x5B = pure $ chr $ fromIntegral b
         | otherwise = printf "|0x%02x|" b
