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
    { pageVideo, pageRAM :: Location
    , borderStore, backgroundStore :: Location
    , blitStore :: Location
    , blitPicture :: Location
    }


-- | Pre: `A` is the border color
-- | Pre: `B` is the background color
-- | Post: `A` is the background color
setColors_ :: Locations -> Z80ASM
setColors_ Locations{..} = mdo
    -- Set border
    out [0x00] A
    ld [borderStore] A

    -- Has the background color changed?
    ld A [backgroundStore]
    cp B
    ret Z

    call pageVideo

    -- Set palette 0 (background) for text
    ld A B
    ld [backgroundStore] A
    out [0x60] A

    -- Fill background of picture area
    push HL

    -- Top
    ld HL videoStart
    decLoopB pictureMarginTop do
        push BC
        decLoopB rowStride do
            ld [HL] A
            inc HL
        pop BC

    -- Sides
    ld DE $ picWidth `div` 2
    decLoopB (picHeight * 2) do
        push BC
        decLoopB pictureMarginLeft do
            ld [HL] A
            inc HL
        add HL DE
        decLoopB pictureMarginLeft do
            ld [HL] A
            inc HL
        pop BC

    -- Bottom
    ld HL $ videoStart + (fromIntegral pictureMarginTop + (picHeight * 2)) * rowStride
    decLoopB pictureMarginBottom do
        push BC
        decLoopB rowStride do
            ld [HL] A
            inc HL
        pop BC

    push AF
    call pageRAM
    pop AF
    pop HL
    ret

pictureMarginTop :: Word8
pictureMarginTop = 8

pictureMarginBottom :: Word8
pictureMarginBottom = 2

pictureMarginLeft :: Word8
pictureMarginLeft = (rowStride - (picWidth `div` 2)) `div` 2

pictureStart :: Word16
pictureStart = videoStart + (fromIntegral pictureMarginTop * rowStride) + fromIntegral pictureMarginLeft

-- | Render the picture as a 80x40 mode-4 rectangle
-- | Pre: `IX`: pointer to colormap
-- | Pre: `HL`: pointer to bitmap
-- | Pre: `IY`: pointer to destination
renderPicture :: Z80ASM
renderPicture = skippable \end -> mdo
    decLoopB (picHeight `div` 8) do
        push BC
        decLoopB 8 do -- 8 lines per colormap row
            push BC
            push IX

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

            pop IX
            pop BC

        ld DE $ picWidth `div` 8
        add IX DE

        pop BC
    jp end

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

blitPicture_ :: Locations -> Z80ASM
blitPicture_ Locations{..}= mdo
    call pageVideo

    ld HL blitStore
    ld DE pictureStart
    decLoopB picHeight do
        push BC

        -- Duplicate each row
        let blitLine = do
                ld BC $ picWidth `div` 2 -- 2 pixels / byte
                ldir
                ex DE HL
                ld BC $ 64 - picWidth `div` 2
                add HL BC
                ex DE HL

        push HL
        blitLine
        pop HL
        blitLine

        pop BC

    jp pageRAM

-- | Pre: `HL` is the start of the picture data (bitmap <> colormap)
displayPicture_ :: Locations -> Z80ASM
displayPicture_ Locations{..} = mdo
    -- Calculate `IX` to point to colormap
    push HL
    pop IX
    ld DE $ picWidth `div` 8 * picHeight
    add IX DE

    ld IY blitStore
    renderPicture
    ret

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

-- | Pre: `IX` is the pointer to sprite state
blitSprite_ :: Locations -> Z80ASM
blitSprite_ Locations{..} = do
    -- Move sprite data to a region outside the video RAM
    ld L [IX + 1]
    ld H [IX + 2]
    ld DE 0x0800
    ld BC $ fromIntegral spriteHeight * fromIntegral spriteStride
    ldir
    ld HL 0x0800

    -- D: sprite X coordinate
    -- E: sprite Y coordinate
    ld D [IX + 4]
    ld E [IX + 5]
    computeSpriteTarget

    call pageVideo

    -- Draw sprite
    -- C: sprite color
    -- HL: pointer to sprite bitmap
    -- IY: pointer to target video memory
    ld C [IX + 3]
    decLoopB spriteHeight do
        push BC

        ld E 2 -- Double scanline counter
        push HL

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
                    rrc C

                    -- Should we change second pixel?
                    sla D
                    unlessFlag NC do
                        Z80.and 0b1010_1010
                        Z80.or C
                        pure ()
                    rlc C

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

    jp pageRAM
