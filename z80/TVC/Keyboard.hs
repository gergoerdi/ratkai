{-# LANGUAGE BlockArguments, NumericUnderscores, BinaryLiterals, RecursiveDo #-}
module TVC.Keyboard where

import Z80
import Z80.Utils

import Data.Word
import Control.Monad

kbdRepeatRate :: Word8
kbdRepeatRate = 3

kbdRepeatWait :: Word8
kbdRepeatWait = 25

kbdBufLen :: Word8
kbdBufLen = 4 -- Has to be a power of 2

scanKeyboard :: Location -> Z80ASM
scanKeyboard kbdBuf= mdo
    -- Scan keyboard
    ld A [0x0b11]
    Z80.and 0xf0
    ld D A
    ld HL kbdState
    decLoopB 10 do
        ld A D
        inc D

        out [0x03] A
        in_ A [0x58]

        ld [HL] A
        inc HL

    -- Compare keyboard state with previous state
    ld HL kbdState
    ld IX kbdPrevState
    ld IY repeatTimer
    ld D 0
    decLoopB 10 do
        ld A [HL]
        cpl
        ld E [IX]
        push BC
        decLoopB 8 mdo
            srl A
            jp NC notHeld
            srl E
            jp C newlyPressed

            ld C [IY]
            dec C
            ld [IY] C
            jp NZ next

            ld [IY] kbdRepeatRate
            jp this

            newlyPressed <- labelled do
                ld [IY] kbdRepeatWait

            this <- labelled do
                pop BC -- To keep stack consistent, since we pushed BC before the `decLoopB 8`
                jp found

            notHeld <- labelled $ srl E
            next <- label
            inc D
        pop BC
        inc HL
        inc IX
    jp copyKbdState

    found <- labelled do
        ld A [kbdBufW]
        inc A
        Z80.and $ kbdBufLen - 1
        ld [kbdBufW] A

        ld HL kbdBuf
        add A L
        ld L A
        unlessFlag NC $ inc H
        ld [HL] D

    copyKbdState <- labelled do
        ld DE kbdPrevState
        ld HL kbdState
        ld BC 10
        ldir

    when False do
        -- Set border color to dark green
        ld A 0b10_10_00_00
        out [0x00] A
    jp end

    repeatTimer <- labelled $ db [kbdRepeatRate]
    kbdBufW <- labelled $ db [0]
    kbdPrevState <- labelled $ db $ replicate 10 0x00
    kbdState <- labelled $ db $ replicate 10 0x00

    end <- label
    pure ()

-- | Post: `A` is character, flag `Z` iff no character is available
readChar_ :: Location -> Location -> Z80ASM
readChar_ keyMap kbdBuf = mdo
    ld A [kbdBufR]
    inc A
    Z80.and $ kbdBufLen - 1
    ld [kbdBufR] A

    ld HL kbdBuf
    add A L
    ld L A
    unlessFlag NC $ inc H
    ld A [HL]
    ld [HL] 0xff
    cp 0xff
    ret Z

    -- push AF
    -- call printByte
    -- pop AF

    ld HL keyMap
    add A L
    ld L A
    unlessFlag NC $ inc H
    ld A [HL]
    cp 0xff
    ret

    kbdBufR <- labelled $ db [0]
    pure ()
