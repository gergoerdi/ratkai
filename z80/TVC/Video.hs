{-# LANGUAGE BlockArguments, NumericUnderscores, BinaryLiterals, RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
module TVC.Video where

import Z80
import Z80.Utils
import TVC

import Data.Word
import Control.Monad

charsPerRow :: Word8
charsPerRow = 31

firstLine :: Word8
firstLine = 12

lastLine :: Word8
lastLine = 30

charHeight :: Word8
charHeight = 8

maxInput :: Word8
maxInput = charsPerRow - 1

rowStride :: Word16
rowStride = 64

data Locations = Locations
    { lineNum, colNum :: Location
    , charset :: Location
    , pageVideoIn, pageVideoOut :: Location
    , drawColorIsInput :: Location
    , newLine :: Location
    }

-- Pre: `C` is the character to print
-- Clobbers `AF`, `BC`, `HL`, `IX`
printCharC4_ :: Locations -> Z80ASM
printCharC4_ Locations{..} = mdo
    skippable \sameLine -> do
        ld A [colNum]
        cp charsPerRow
        jp C sameLine
        push BC
        call newLine
        pop BC

    -- A = 8 * lineNum
    ld A [lineNum]
    sub 0
    replicateM_ 3 rla

    -- HL = 64 * (8 * lineNum)
    ld H 0
    ld L A
    decLoopB 6 do
        sla L
        rl H

    -- HL += 2 * colNum
    ld A [colNum]
    sla A
    inc A
    add A L
    unlessFlag NC $ inc H
    ld L A

    -- HL += videoStart
    ld DE videoStart
    add HL DE

    -- Increase column number
    ld A [colNum]
    inc A
    ld [colNum] A

    -- Search for character start address
    ld D 0
    ld E C
    sub 0
    replicateM_ 3 $ do
        rl E
        rl D
    ld IX charset
    add IX DE

    call pageVideoIn

    ld A [drawColorIsInput]
    cp 0x00
    ld DE $ rowStride - 1
    jp NZ drawInput

    drawOutput <- labelled do
        decLoopB 8 mdo
            ld A [IX]
            inc IX
            ld C A

            -- Byte 1: First four pixels
            Z80.and 0xf0
            ld [HL] A
            inc HL

            ld A C
            replicateM_ 4 rla
            Z80.and 0xf0

            ld [HL] A
            add HL DE
        jp drawDone

    drawInput <- labelled do
        decLoopB 8 mdo
            ld A [IX]
            inc IX
            ld C A

            -- Byte 1: First four pixels
            replicateM_ 4 $ srl A
            ld [HL] A
            inc HL

            ld A C
            Z80.and 0x0f
            -- replicateM_ 4 rla
            -- Z80.and 0xf0

            ld [HL] A
            add HL DE
        jp drawDone


    drawDone <- label
    jp pageVideoOut
    -- syscall 0x01
    ret

newLine_ :: Locations -> Z80ASM
newLine_ Locations{..} = do
    ldVia A [colNum] 0
    ld A [lineNum]
    inc A
    cp lastLine
    unlessFlag Z do
        ld [lineNum] A
        ret

    push BC
    push DE
    push HL
    call pageVideoIn
    ld HL $ videoStart + (fromIntegral firstLine + 1) * rowStride * fromIntegral charHeight
    ld DE $ videoStart + fromIntegral firstLine * rowStride * fromIntegral charHeight
    ld BC $ (fromIntegral $ lastLine - firstLine) * rowStride * fromIntegral charHeight
    ldir
    call pageVideoOut
    pop HL
    pop DE
    pop BC
    ret

setMainColor_ :: Locations -> Z80ASM
setMainColor_ Locations{..} = do
    ldVia A [drawColorIsInput] 0x00
    ret

setInputColor_ :: Locations -> Z80ASM
setInputColor_ Locations{..} = do
    ldVia A [drawColorIsInput] 0xff
    ret

printBack_ :: Locations -> Z80ASM
printBack_ Locations{..} = do
    ld A [colNum]
    dec A
    unlessFlag NC $ ld A 0
    ld [colNum] A
    ret

picWidth :: (Num a) => a
picWidth = 80

picHeight :: (Num a) => a
picHeight = 40

-- | Pre: HL is the start of the picture data (colormap <> bitmap)
-- | Pre: A is the background color
displayPicture_ :: Locations -> Z80ASM
displayPicture_ Locations{..} = mdo
    push AF
    call pageVideoIn
    pop AF

    -- Set palette 0 (background) for text
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
