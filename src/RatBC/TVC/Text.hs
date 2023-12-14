{-# LANGUAGE LambdaCase #-}
module RatBC.TVC.Text where

import Data.Word
import Data.Char (ord, chr, isLower, toUpper)

tvcChar :: Char -> Word8
tvcChar = \case
    '\n' -> 0xf0
    'Á' -> 0x6d
    'É' -> 0x6c
    'Í' -> 0x61
    'Ó' -> 0x76
    'Ö' -> 0x00
    'Ő' -> 0x1b
    'Ú' -> 0x75
    'Ü' -> 0x40
    'Ű' -> 0x1d
    'á' -> 0x70
    'é' -> 0x71
    'í' -> 0x62
    'ó' -> 0x79
    'ö' -> 0x00
    'ő' -> 0x64
    'ú' -> 0x78
    'ü' -> 0x2a
    'ű' -> 0x5f
    '_' -> 0x6f
    c | isLower c -> fromIntegral (ord c) - 0x60
      | otherwise -> fromIntegral (ord c)

fromTVCChar :: Word8 -> Char
fromTVCChar = \case
    0xf0 -> '\n'
    0x6d -> 'Á'
    0x6c -> 'É'
    0x61 -> 'Í'
    0x76 -> 'Ó'
    0x1b -> 'Ő'
    0x75 -> 'Ú'
    0x40 -> 'Ü'
    0x1d -> 'Ű'
    0x70 -> 'á'
    0x71 -> 'é'
    0x62 -> 'í'
    0x79 -> 'ó'
    0x00 -> 'Ö'
    0x64 -> 'ő'
    0x78 -> 'ú'
    0x2a -> 'ü'
    0x5f -> 'ű'
    0x6f -> '_'
    x | x' <- fromIntegral x + 0x60
      , x' < 0x80
      , c <- chr x'
      , isLower c -> c
      | otherwise -> chr (fromIntegral x)

keymap :: [(Word8, Word8)]
keymap = map (tvcChar . toUpper <$>)
    [ (0x05, 'í')
    , (0x06, '1')
    , (0x02, '2')
    , (0x01, '3')
    , (0x07, '4')
    , (0x00, '5')
    , (0x04, '6')
    , (0x0f, '7')
    , (0x09, '8')
    , (0x0a, '9')
    , (0x03, '0')
    , (0x0b, 'ü')
    , (0x0e, 'ö')
    , (0x0d, 'ó')
    , (0x28, '\DEL')
    , (0x16, 'q')
    , (0x12, 'w')
    , (0x11, 'e')
    , (0x17, 'r')
    , (0x10, 't')
    , (0x36, 'y')
    , (0x1f, 'u')
    , (0x19, 'i')
    , (0x1a, 'o')
    , (0x1e, 'p')
    , (0x1b, 'ő')
    , (0x1d, 'ú')
    , (0x2d, 'ű')
    , (0x26, 'a')
    , (0x22, 's')
    , (0x21, 'd')
    , (0x27, 'f')
    , (0x20, 'g')
    , (0x24, 'h')
    , (0x2f, 'j')
    , (0x29, 'k')
    , (0x2a, 'l')
    , (0x2b, 'á')
    , (0x2e, 'é')
    , (0x14, 'z')
    , (0x32, 'x')
    , (0x31, 'c')
    , (0x37, 'v')
    , (0x30, 'b')
    , (0x34, 'n')
    , (0x3f, 'm')
    , (0x3d, ' ')
    , (0x2c, '\n')
    , (0x39, ',')
    , (0x3a, '.')
    ]
