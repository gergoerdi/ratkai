{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
module Ratkai.TVC.Picture where

import TVC
import RatBC.Picture
import RatBC.TVC.Picture

import Z80
import Z80.Utils

import Data.Bifunctor
import Control.Monad
import qualified Data.ByteString as BS
import Data.Word
import Data.Array (Array, (!), listArray)
import Data.Bits

picWidth :: (Num a) => a
picWidth = 80

picHeight :: (Num a) => a
picHeight = 40

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

    colormap' = BS.map toTVCColors colormap

    toTVCColors :: Word8 -> Word8
    toTVCColors = fromNybbles . both toTVCColor . nybbles

fromNybbles :: (Word8, Word8) -> Word8
fromNybbles (n1, n0) = (n1 `shiftL` 4) .|. (n0 `shiftL` 0)

data Locations = Locations
    { pageVideoIn, pageVideoOut :: Location
    }

-- | Pre: `HL` is the start of the picture data (colormap <> bitmap)
-- | Pre: `A` is the border color
-- | Pre: `B` is the background color
displayPicture_ :: Locations -> Z80ASM
displayPicture_ Locations{..} = mdo
    -- Set border
    out [0x00] A

    call pageVideoIn

    -- Set palette 0 (background) for text
    ld A B
    out [0x60] A

    -- Fill background of picture area
    ld DE videoStart
    decLoopB 90 do
        push BC
        decLoopB 64 do
            ld [DE] A
            inc DE
        pop BC
    let nextRow = do
            ld A (64 - 40)
            add A E
            ld E A
            unlessFlag NC $ inc D

    -- Draw picture
    -- IX: pointer to colormap
    -- HL: pointer to bitmap
    -- IY: pointer to video memory
    push HL
    pop IX
    ld DE $ picWidth `div` 8 * picHeight `div` 8
    add HL DE
    ld IY $ videoStart + (8 * 64) + ((64 - 40) `div` 2)
    decLoopB (picHeight `div` 8) do
        push BC
        decLoopB 8 do -- 4 double scanlines per colormap row
            push BC
            push IX

            ld E 2 -- Double scanline counter
            push HL
            push IX
            withLabel \loop -> do
                decLoopB (picWidth `div` 8) do -- One scanline
                    push BC
                    ld A [HL]
                    decLoopB 4 do -- 4 * 2 bits per byte
                        push BC

                        -- Shift out even bit's color into C
                        call shiftOutPixel
                        ld C A

                        ld A B
                        -- Shift out odd bit's color into D
                        call shiftOutPixel
                        ld D A

                        -- Combine C and D into A
                        ld A C
                        rla
                        Z80.or D

                        ld [IY] A
                        inc IY

                        -- Restore A to get ready for next two bits
                        ld A B

                        pop BC
                    inc HL
                    inc IX

                    pop BC

                push DE
                ld DE (64 - picWidth `div` 2)
                add IY DE
                pop DE

                dec E
                unlessFlag Z do
                    pop IX
                    pop HL
                    jp loop

            pop IX
            pop BC

        ld DE (picWidth `div` 8)
        add IX DE

        pop BC

    jp pageVideoOut

    shiftOutPixel <- labelled do
        -- Shift out bit, use it as an index into colormap's two nybbles
        rla
        ld B A
        ld A [IX]
        unlessFlag NC do
            replicateM_ 4 rra
        Z80.and 0x0f

        -- Spread out A's value using the lookup table `spreads`
        push DE
        ld DE spreads
        add A E
        ld E A
        unlessFlag NC $ inc D
        ld A [DE]
        pop DE
        ret

        -- ld A 0b01_01_01_01
        -- ret C
        -- ld A 0b00_00_00_00
        -- ret

    spreads <- labelled $ db [spread x | x <- [0..15]]

    pure ()
