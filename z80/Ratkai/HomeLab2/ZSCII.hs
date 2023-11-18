{-# LANGUAGE RecordWildCards, NamedFieldPuns, RecursiveDo, BlockArguments #-}
{-# LANGUAGE BinaryLiterals, NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Ratkai.HomeLab2.ZSCII where

import Z80
import Z80.Utils
import Control.Monad

-- | Unpack a ZSCII pair of bytes from [IX] into [unpackBuf], and set [unpackIsLast]
--   HL is incremented by 2 in the process.
unpackZ_ :: Location -> Location -> Z80ASM
unpackZ_ unpackBuf unpackIsLast = mdo
    push DE
    ldVia A E [IX]
    inc IX

    -- Third character: lowest 5 bits of E
    Z80.and 0x1f
    ld [unpackBuf + 2] A

    ldVia A D [IX]
    inc IX

    -- Second character: lowest 2 bits of D with highest 3 bits of E
    replicateM_ 5 $ srl E
    replicateM_ 3 $ sla A
    Z80.or E
    Z80.and 0x1f
    ld [unpackBuf + 1] A

    -- first character: bits 6..2 of D
    ld A D
    replicateM_ 2 $ srl A
    Z80.and 0x1f
    ld [unpackBuf + 0] A

    -- Finished: bit 7 of D
    ld A D
    Z80.and 0b1000_0000
    ld [unpackIsLast] A

    pop DE
    ret

-- | Decode a single character in ZSCII codepage in A.
--   Sets Z iff the character is unprintable (i.e. a shift)
decodeZ1_ shiftState = mdo
    push AF
    ld A [shiftState]
    cp 0
    ldVia A [shiftState] 0
    jr NZ shifted
    pop AF

    cp 0
    ret Z

    cp 1
    jr Z shift

    cp 2
    jr Z space
    cp 3
    jr Z period
    cp 4
    jr Z comma
    cp 5
    jr Z newline

    add A (0x41 - 6)
    printable <- labelled do
        cp 0 -- Clear Z flag
        ret

    space <- labelled do
        ld A 0x20
        jr printable
    period <- labelled do
        ld A 0x2e
        jr printable
    comma <- labelled do
        ld A 0x2c
        jr printable
    newline <- labelled do
        ld A 0x0d
        jr printable

    shifted <- labelled mdo
        pop AF
        sub 1
        ret C
        cp 22
        jp C symbol
        add A (0x30 - 22 + 1)
        jr printable

    symbol <- labelled mdo
        push IX
        push DE
        ld IX symbols
        ld D 0
        ld E A
        add IX DE
        ld A [IX]
        pop DE
        pop IX
        jr printable

    symbols <- labelled $ db [0x3f, 0x27, 0x3a, 0x2d, 0x26, 0x21, 0x0c, 0x0e]

    shift <- labelled do
        ldVia A [shiftState] 1
        setZ
        ret
    pure ()
