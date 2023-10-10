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

        ld HL text2
        ld B 163
        call printlnZ

        ld HL text2
        ld B 175
        call printlnZ

        loopForever $ pure ()
        printlnZ <- labelled $ do
            call printZ
            ld A 0x0d
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
                ldVia A E [HL]
                inc HL

                -- Third character: lowest 5 bits of E
                Z80.and 0x1f
                ld [buf + 2] A

                ldVia A D [HL]
                inc HL

                -- Second character: lowest 2 bits of D with highest 3 bits of E
                replicateM_ 5 $ srl E
                replicateM_ 3 $ sla A
                Z80.or E
                Z80.and 0x1f
                ld [buf + 1] A

                -- first character: bits 6..2 of D
                ld A D
                replicateM_ 2 $ srl A
                Z80.and 0x1f
                ld [buf + 0] A

                -- Finished: bit 7 of D
                ld A D
                Z80.and 0b1000_0000
                ld [isLast] A

                ld IX buf
                replicateM_ 3 $ do
                    call printZ1
                    inc IX
                ld A [isLast]
                cp 0
                ret NZ
            pure ()

        -- Print a single character in ZSCII codepage from [IX]
        printZ1 <- labelled mdo
            ld A [shiftState]
            cp 0
            ldVia A [shiftState] 0
            jr NZ shifted

            ld A [IX]
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
            jr Z bang

            add A (0x41 - 6)
            printA <- labelled do
                rst 0x28
                ret

            space <- labelled do
                ld A 0x20
                jr printA
            period <- labelled do
                ld A 0x2e
                jr printA
            comma <- labelled do
                ld A 0x2c
                jr printA
            bang <- labelled do
                ld A 0x21
                jr printA

            shifted <- labelled mdo
                ld A [IX]
                -- TODO: it could be something other than space
                sub 1
                ret C
                cp 5
                jp C symbol
                add A (0x30 - 5)
                jr printA

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
                    jp 0x01fc
                    symbols <- labelled $ db [0x21, 0x3f, 0x27, 0x3a, 0x2d, 0x26]
                    pure ()
                pure ()

            shift <- labelled do
                ldVia A [shiftState] 1
            ret

        text1 <- labelled $ db text1'
        text2 <- labelled $ db text2'
        dict <- labelled $ db dict'
        scriptEnter <- labelled $ db scriptEnter'
        scriptAfter <- labelled $ db scriptAfter'
        scriptGlobal <- labelled $ db scriptGlobal'
        scriptLocal <-  labelled $ db scriptLocal'
        help <- labelled $ db help'
        reset <- labelled $ db reset'

        buf <- labelled $ db [0, 0, 0]
        isLast <- labelled $ db [0]
        shiftState <- labelled $ db [0]

        pure ()
