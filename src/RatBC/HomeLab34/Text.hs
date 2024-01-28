module RatBC.HomeLab34.Text where

import Data.Word
import Data.Char (ord)

encodeChar :: Char -> Word8
encodeChar = \case
    '\n' -> encodeChar ' '
    'Á' -> 0x40
    'É' -> 0x5b
    'Í' -> encodeChar 'I'
    'Ó' -> 0x5f
    'Ö' -> 0x5c
    'Ő' -> encodeChar 'Ö'
    'Ú' -> encodeChar 'U'
    'Ü' -> 0x5d
    'Ű' -> encodeChar 'Ü'
    'á' -> 0x7f
    'é' -> 0x7b
    'í' -> encodeChar 'i'
    'ó' -> 0x7e
    'ö' -> 0x7c
    'ő' -> encodeChar 'ö'
    'ú' -> encodeChar 'u'
    'ü' -> 0x7d
    'ű' -> encodeChar 'ü'
    '_' -> 0x6f
    c -> fromIntegral . ord $ c
