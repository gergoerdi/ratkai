{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
module Ratkai.TVC.Picture where

-- import Z80
-- import Z80.Utils
-- import qualified Data.ByteString as BS
-- import Data.String (fromString)
-- import System.FilePath
-- import System.Directory
-- import Text.Printf
import qualified Data.ByteString as BS
import Data.Word
import Data.Array (Array, (!), listArray)
import Data.Bits

picWidth :: (Num a) => a
picWidth = 80

picHeight :: (Num a) => a
picHeight = 40

reorder :: Int -> Int -> Int -> [a] -> [a]
reorder stride w h xs = map (xs!!) $
    [ (stride * y0) + (x * 8 + y)
    | y0 <- [0 .. (h `div` 8) - 1]
    , y <- [0..7]
    , x <- [0..(w `div` 8) -1]
    ]

picData :: Word8 -> BS.ByteString -> BS.ByteString
picData picNum bs = colormap' <> reorder' bitmap
  where
    reorder' = BS.pack . reorder picWidth picWidth picHeight . BS.unpack

    size = picHeight * (picWidth `div` 8)
    colorSize = size `div` 8
    bitmapAddr = 0xa000 + fromIntegral (picNum - 1) * (size + colorSize)
    colormapAddr = bitmapAddr + size
    bitmap = BS.take size . BS.drop bitmapAddr $ bs
    colormap = BS.take colorSize . BS.drop colormapAddr $ bs

    colormap' = BS.map toTVCColor colormap

    toTVCColor :: Word8 -> Word8
    toTVCColor = fromNybbles . both (tvcPalette !) . nybbles

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

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

nybbles :: Word8 -> (Word8, Word8)
nybbles b = both (.&. 0x0f) (n1, n0)
  where
    n1 = b `shiftR` 4
    n0 = b `shiftR` 0

fromNybbles :: (Word8, Word8) -> Word8
fromNybbles (n1, n0) = (n1 `shiftL` 4) .|. (n0 `shiftL` 0)
