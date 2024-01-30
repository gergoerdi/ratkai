{-# LANGUAGE TupleSections, OverloadedStrings #-}
module RatBC.Text (loadMessages, wrapWords) where

import RatBC.Utils

import qualified Data.ByteString.Lazy as BL
import Data.Char
import Text.Printf (printf)
import Data.Word
import Data.List (isPrefixOf)
import Data.List.Split

loadMessages :: BL.ByteString -> ([String], [String])
loadMessages bs = (f bank1, f bank2)
  where
    f ptr =
        takeWhile (not . ("NTR" `isPrefixOf`)) .
        take 256 .
        map (concatMap toChar . BL.unpack) .
        BL.split 0xFF $
        deref bs ptr

    bank1 = 0x082f
    bank2 = 0x0831

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

         -- | b == 0x00 = "o"
         -- | b == 0x1b = "O"
         -- | b == 0x2a = "u"
         -- | b == 0x40 = "U"
         -- | b == 0x5f = "u"
         -- | b == 0x61 = "I"
         -- | b == 0x62 = "i"
         -- | b == 0x64 = "o"
         -- | b == 0x6c = "E"
         -- | b == 0x6d = "A"
         -- | b == 0x70 = "a"
         -- | b == 0x71 = "e"
         -- | b == 0x75 = "U"
         -- | b == 0x78 = "u"
         -- | b == 0x79 = "o"
         -- | b == 0x7a = "O"
         | 0x01 <= b && b < 0x1B = pure $ chr $ fromIntegral b + 0x60
         | 0x20 <= b && b < 0x5B = pure $ chr $ fromIntegral b
         -- | otherwise = printf "|0x%02x|" b
         | otherwise = ""

wrapWords :: Int -> String -> String
wrapWords cols = foldMap wrapLine . split (onSublist "\n")
  where
    wrapLine para = startLine tokens
      where
        tokens = split (dropBlanks $ dropDelims $ onSublist " ") $ para

    startLine = reflow True cols

    reflow :: Bool -> Int -> [String] -> String
    reflow first n [] = []
    reflow first n (s:ss)
        | k == n = space <> s <> startLine ss              -- Automatic line-wrapping on last column
        | k == n - 1 = space <> s <> " " <> startLine ss   -- Automatic line-wrapping on last column
        | k < n = space <> s <> more ss                   -- Keep going
        | k > cols = "\n" <> s <> "\n" <> startLine ss    -- This word needs its own line
        | otherwise = "\n" <> startLine (s:ss)            -- Start a new line
      where
        k = length (space <> s)
        space = if first then "" else " "
        more = reflow False (n - k)
