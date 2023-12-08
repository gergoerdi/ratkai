{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
module Ratkai.TVC.Picture where

import Z80
import Z80.Utils

import Z80.TVC
import RatBC.Picture
import RatBC.TVC.Picture

import Data.Bifunctor
import Control.Monad
import qualified Data.ByteString as BS
import Data.Word
import Data.Array (Array, (!), listArray)
import Data.Bits

data Locations = Locations
    { pageVideoIn, pageVideoOut :: Location
    }


-- | Pre: `A` is the border color
-- | Pre: `B` is the background color
setColors_ :: Locations -> Z80ASM
setColors_ Locations{..} = mdo
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

    jp pageVideoOut

-- | Pre: `HL` is the start of the picture data (bitmap <> colormap)
displayPicture_ :: Locations -> Z80ASM
displayPicture_ Locations{..} = mdo
    -- Move picture data to a region outside the video RAM
    push BC
    push DE
    ld DE 0x0800
    ld BC 450 -- TODO compute this nicer
    ldir
    pop DE
    pop BC
    ld HL 0x0800

    call pageVideoIn

    -- Draw picture
    -- IX: pointer to colormap
    -- HL: pointer to bitmap
    -- IY: pointer to video memory
    push HL
    pop IX
    ld DE $ picWidth `div` 8 * picHeight
    add IX DE
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
