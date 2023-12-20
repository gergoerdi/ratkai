{-# LANGUAGE BlockArguments, NumericUnderscores, BinaryLiterals, RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
module Z80.TVC.Video where

import Z80
import Z80.Utils
import Z80.TVC

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

rowStride :: Num a => a
rowStride = 64

data Locations = Locations
    { lineNum, colNum :: Location
    , charset :: Location
    , pageVideo, pageRAM :: Location
    , drawColorIsInput :: Location
    , newLine :: Location
    }

-- Pre: `C` is the character to print
-- Clobbers `AF`, `BC`, `DE`, `HL`, `IX`
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

    call pageVideo

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
    jp pageRAM
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
    call pageVideo
    ld HL $ videoStart + (fromIntegral firstLine + 1) * rowStride * fromIntegral charHeight
    ld DE $ videoStart + fromIntegral firstLine * rowStride * fromIntegral charHeight
    ld BC $ (fromIntegral $ lastLine - firstLine) * rowStride * fromIntegral charHeight
    ldir
    call pageRAM
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
