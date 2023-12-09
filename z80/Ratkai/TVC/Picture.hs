{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
module Ratkai.TVC.Picture where

import Z80
import Z80.Utils

import Z80.TVC
import Z80.TVC.Video (rowStride)
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
        decLoopB rowStride do
            ld [DE] A
            inc DE
        pop BC

    jp pageVideoOut

pictureStart :: Word16
pictureStart = videoStart + (8 * rowStride) + ((rowStride - 40) `div` 2)

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
    ld IY pictureStart
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

spriteHeight = 21
spriteStride = 3
spriteWidth = spriteStride * 8

-- | Pre: `D` is sprite X coordinate
-- | Pre: `E` is sprite Y coordinate
-- | Post: `IY` is pointer to video memory
-- | Clobbers `B`, `C`, `E`
computeSpriteTarget :: Z80ASM
computeSpriteTarget = do
    -- Compute target address
    ld IY pictureStart

    -- Apply Y coordinate: add rowStride * E to IY
    ld BC rowStride
    replicateM_ 8 do
        srl E
        unlessFlag NC $ add IY BC
        sla C
        rl B

    -- Apply X coordinate: add D to IY
    ld B 0
    ld C D
    add IY BC

-- | Pre: `C` is sprite color
-- | Pre: `D` is sprite X coordinate
-- | Pre: `E` is sprite Y coordinate
-- | Pre: `HL` is the start of the sprite bitmap
-- | Pre: `IX` is the start of the sprite backing store
displaySprite_ :: Locations -> Z80ASM
displaySprite_ Locations{..} = do
    -- Move sprite data to a region outside the video RAM
    push BC
    push DE
    ld DE 0x0800
    ld BC $ fromIntegral spriteHeight * fromIntegral spriteStride
    ldir
    pop DE
    ld HL 0x0800

    -- Save X and Y coordinate
    ld [IX] D
    inc IX
    ld [IX] E
    inc IX

    computeSpriteTarget
    pop BC

    call pageVideoIn

    -- Draw sprite
    -- HL: pointer to sprite bitmap
    -- IY: pointer to target video memory
    -- IX: pointer to sprite backing store
    decLoopB spriteHeight do
        push BC

        ld E 2 -- Double scanline counter
        push HL

        push IY
        decLoopB (fromIntegral spriteWidth `div` 2) do
            ld A [IY]
            inc IY
            ld [IX] A
            inc IX
        pop IY

        withLabel \loop -> do
            decLoopB spriteStride do
                ld D [HL] -- Load next 8 pixels
                inc HL
                push BC
                decLoopB 4 do
                    ld A [IY] -- Load two background pixels
                    -- Should we change first pixel?
                    sla D
                    unlessFlag NC do
                        Z80.and 0b0101_0101
                        Z80.or C
                        pure ()
                    rlc C

                    -- Should we change second pixel?
                    sla D
                    unlessFlag NC do
                        Z80.and 0b1010_1010
                        Z80.or C
                        pure ()
                    rrc C

                    ld [IY] A
                    inc IY
                pop BC

            push DE
            ld DE $ (rowStride :: Word16) - fromIntegral spriteWidth `div` 2
            add IY DE
            pop DE

            dec E
            unlessFlag Z do
                pop HL
                jp loop

        pop BC

    jp pageVideoOut

-- | Pre: `IX` is the start of the sprite backing store
hideSprite_ :: Locations -> Z80ASM
hideSprite_ Locations{..} = do
    -- Retrieve X and Y coordinate
    ld D [IX]
    inc IX
    ld E [IX]
    inc IX

    -- Compute target address
    computeSpriteTarget

    call pageVideoIn

    -- Clear sprite
    -- IY: pointer to target video memory
    -- IX: pointer to sprite backing store
    decLoopB spriteHeight do
        push BC

        ld E 2 -- Double scanline counter
        push IX

        withLabel \loop -> do
            decLoopB (fromIntegral spriteWidth `div` 2) do
                ld A [IX]
                inc IX
                ld [IY] A
                inc IY

            push DE
            ld DE $ (rowStride :: Word16) - fromIntegral spriteWidth `div` 2
            add IY DE
            pop DE

            dec E
            unlessFlag Z do
                pop IX
                jp loop
        pop BC

    jp pageVideoOut
