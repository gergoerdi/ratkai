module RatBC.Picture where

import qualified Data.ByteString as BS
import Data.Bits
import Data.Word

type RGBA8 = (Word8, Word8, Word8, Word8)

picWidth :: (Num a) => a
picWidth = 80

picHeight :: (Num a) => a
picHeight = 40

picRowstride :: (Num a) => a
picRowstride = 80

hiresPixels :: BS.ByteString -> BS.ByteString -> [Word8]
hiresPixels = hiresPixels' picWidth picHeight picRowstride

hiresPixels' :: Int -> Int -> Int -> BS.ByteString -> BS.ByteString -> [Word8]
hiresPixels' w h rowstride colors12 bitmap =
    concatMap (uncurry applyPalette) $
    zip (reorder w h rowstride $ toColorMap colors12) $
    reorder w h rowstride $ BS.unpack bitmap
  where
    toColorMap = concatMap (replicate 8) . BS.unpack

    applyPalette :: Word8 -> Word8 -> [Word8]
    applyPalette c12 = map toColor . bits
      where
        (c1, c2) = nybbles c12

        toColor True = c1
        toColor False = c2

type RGB8 = (Word8, Word8, Word8)

-- | Based on http://www.pepto.de/projects/colorvic/
paletteC64 :: Word8 -> RGB8
paletteC64 c = (!!(fromIntegral c .&. 0x0f))
    [ (0x00, 0x00, 0x00)
    , (0xff, 0xff, 0xff)
    , (0x89, 0x40, 0x36)
    , (0x7A, 0xBF, 0xC7)
    , (0x8a, 0x46, 0xae)
    , (0x68, 0xa9, 0x41)
    , (0x3e, 0x31, 0xa2)
    , (0xd0, 0xdc, 0x71)
    , (0x90, 0x5f, 0x25)
    , (0x5c, 0x47, 0x00)
    , (0xbb, 0x77, 0x6d)
    , (0x55, 0x55, 0x55)
    , (0x80, 0x80, 0x80)
    , (0xac, 0xce, 0x88)
    , (0x7c, 0x70, 0xda)
    , (0xab, 0xab, 0xab)
    ]

reorder :: Int -> Int -> Int -> [a] -> [a]
reorder w h stride xs = map (xs!!) $
    [ (stride * y0) + (x * 8 + y)
    | y0 <- [0 .. (h `div` 8) - 1]
    , y <- [0..7]
    , x <- [0..(w `div` 8) -1]
    ]

bits :: Word8 -> [Bool]
bits b = map (testBit b) $ reverse [0..7]

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

nybbles :: Word8 -> (Word8, Word8)
nybbles b = both (.&. 0x0f) (n1, n0)
  where
    n1 = b `shiftR` 4
    n0 = b `shiftR` 0
