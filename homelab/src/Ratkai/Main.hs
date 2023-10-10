{-# LANGUAGE RecordWildCards, RecursiveDo, BlockArguments #-}
{-# LANGUAGE BinaryLiterals, NumericUnderscores #-}
module Ratkai.Main (game) where

import Z80
import Z80.Utils
import HL2
import qualified Data.ByteString as BS
import Control.Monad
import System.FilePath

game :: IO Z80ASM
game = do
    let asset name = BS.readFile $ "/home/cactus/prog/c64/bosszu-disasm/ratbc/_out/hl2-ep1.strip/" </> name <.> "bin"
    text1' <- asset "text1"
    text2' <- asset "text2"
    dict' <- asset "dict"
    scriptEnter' <- asset "enter"
    scriptAfter' <- asset "after"
    scriptGlobal' <- asset "interactive-global"
    scriptLocal' <- asset "interactive-local"
    help' <- asset "help"
    reset' <- asset "reset"

    pure $ mdo
        ldVia A [shiftState] 0

        ld HL text2
        ld B 1
        call printlnZ

        ld HL text2
        ld B 160
        call printlnZ

        -- ld HL text2
        -- ld B 163
        -- call printlnZ

        -- ld HL text2
        -- ld B 175
        -- call printlnZ

        -- ld HL dict
        -- ld B 0x30
        -- call printZ

        ld HL inputBuf
        call inputLine
        ld A 0x0d
        rst 0x28
        ld HL inputBuf
        withLabel \loop -> do
            ld A [HL]
            rst 0x28
            inc HL
            cp 0
            jp NZ loop

        loopForever $ pure ()

        -- Input one line of text (up to 38 characters), store result in [HL]
        -- Mangles `HL`, `A` and `B`
        inputLine <- labelled $ mdo
            ld A 0x29
            rst 0x28
            ld A 0x20
            rst 0x28
            ld B 38
            withLabel \loop -> mdo
                ld [HL] 0x00
                rst 0x18
                cp 0x0d -- End of line
                ret Z
                cp 0x07 -- Backspace
                jp Z backspace

                -- Normal character: print and record
                dec B
                jr Z noMoreSpace
                rst 0x28
                ld [HL] A
                inc HL
                jr loop

                noMoreSpace <- labelled do
                    inc B -- So that next `dec B` will trigger `Z` again
                    dec HL
                    ld [HL] A
                    ld A 0x07 -- Erase previous last character
                    rst 0x28
                    ld A [HL] -- Print new last character
                    inc HL
                    rst 0x28
                    jr loop

                backspace <- labelled do
                    rst 0x28
                    ld [HL] 0x00
                    dec HL
                    inc B
                    jr loop
                pure ()

        printlnZ <- labelled $ do
            call printZ
            ld A 0x0d
            rst 0x28
            rst 0x28
            ret

        printZ <- labelled $ withLabel \tryNext -> mdo
            -- Is this the message we want?
            dec B
            jr Z start

            loopForever do
                inc HL
                ld A [HL]
                inc HL
                bit 7 A
                jr NZ tryNext

            start <- labelled $ loopForever do
                call unpackZ
                ld IX unpackBuf
                replicateM_ 3 $ do
                    -- call printZ1
                    ld A [IX]
                    inc IX
                    call decodeZ1
                    skippable \unprintable -> do
                        jr Z unprintable
                        call 0x28
                ld A [unpackIsLast]
                cp 0
                ret NZ
            pure ()

        -- Unpack a ZSCII pair of bytes from [HL] into [unpackBuf], and set [unpackIsLast]
        -- HL is incremented by 2 in the process.
        unpackZ <- labelled mdo
            ldVia A E [HL]
            inc HL

            -- Third character: lowest 5 bits of E
            Z80.and 0x1f
            ld [unpackBuf + 2] A

            ldVia A D [HL]
            inc HL

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

            ret

        -- Decode a single character in ZSCII codepage in A.
        -- Sets Z iff the character is unprintable (i.e. a shift)
        decodeZ1 <- labelled mdo
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
                    push HL
                    push DE
                    ld HL symbols
                    ld D 0
                    ld E A
                    add HL DE
                    ld A [HL]
                    pop DE
                    pop HL
                    jr printable
                    symbols <- labelled $ db [0x3f, 0x27, 0x3a, 0x2d, 0x26, 0x21]
                    pure ()
                pure ()

            shift <- labelled do
                ldVia A [shiftState] 1
                setZ
                ret
            pure ()

        text1 <- labelled $ db text1'
        text2 <- labelled $ db text2'
        dict <- labelled $ db dict'
        scriptEnter <- labelled $ db scriptEnter'
        scriptAfter <- labelled $ db scriptAfter'
        scriptGlobal <- labelled $ db scriptGlobal'
        scriptLocal <-  labelled $ db scriptLocal'
        help <- labelled $ db help'
        reset <- labelled $ db reset'

        unpackBuf <- labelled $ db [0, 0, 0]
        unpackIsLast <- labelled $ db [0]
        shiftState <- labelled $ db [0]
        inputBuf <- labelled $ resb 40

        pure ()
