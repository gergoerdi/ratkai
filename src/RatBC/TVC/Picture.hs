{-# LANGUAGE BinaryLiterals #-}
module RatBC.TVC.Picture (toTVCColor, toBorderColor, toBackgroundColor) where

import Data.Word
import Data.Array (Array, listArray, (!))
import Data.Bits

toTVCColor :: Word8 -> Word8
toTVCColor = (tvcPalette !)
  where
    tvcPalette :: Array Word8 Word8
    tvcPalette = listArray (0, 15) . map toIGRB $
        [ (False, False, False, False)
        , (True,  True,  True,  True)
        , (True,  False, True,  True)
        , (False, True,  True,  True)
        , (True,  False, True,  False)
        , (False, True,  False, True)
        , (False, False, True,  True)
        , (True,  True,  False, True)
        , (True,  True,  False, False)
        , (True,  False, False, False)
        , (True,  False, False, True)
        , (True,  True,  False, False)
        , (True,  True,  True,  False)
        , (True,  True,  False, False)
        , (False, True,  True,  False)
        , (True,  True,  True,  True)
        ]

    toIGRB :: (Bool, Bool, Bool, Bool) -> Word8
    toIGRB (r, g, b, i) =
      (if i then 0b1000 else 0b0000) .|.
      (if g then 0b0100 else 0b0000) .|.
      (if r then 0b0010 else 0b0000) .|.
      (if b then 0b0001 else 0b0000)

toBorderColor :: Word8 -> Word8
toBorderColor = (`interleave` 0x00) . toTVCColor

toBackgroundColor :: Word8 -> Word8
toBackgroundColor c = let c' = toTVCColor c in interleave c' c'

interleave :: Word8 -> Word8 -> Word8
interleave x y = (x' `shiftL` 1) .|. y'
  where
    x' = spread x
    y' = spread y

-- https://graphics.stanford.edu/~seander/bithacks.html#InterleaveBMN
spread :: Word8 -> Word8
spread b = foldr (\(s, m) b -> (b .|. (b `shiftL` s)) .&. m) b (zip [1, 2, 4] [0x55, 0x33, 0x0f])
