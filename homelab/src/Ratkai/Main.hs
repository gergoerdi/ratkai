{-# LANGUAGE RecordWildCards, RecursiveDo, BlockArguments #-}
{-# LANGUAGE BinaryLiterals, NumericUnderscores #-}
module Ratkai.Main (game) where

import Z80
import Z80.Utils
import HL2
import qualified Data.ByteString as BS
import Control.Monad

game :: IO Z80ASM
game = do
    text1' <- BS.readFile "/home/cactus/prog/c64/bosszu-disasm/ratbc/_out/text1.zscii"

    pure $ mdo
        ldVia A [shiftState] 0
        ld HL text1
        ld B 29
        call printZ

        loopForever $ pure ()
        printZ <- labelled $ withLabel \tryNext -> mdo
            -- Is this the word we want?
            dec B
            jp Z start -- TODO: jr?

            loopForever do
                inc HL
                ld A [HL]
                inc HL
                bit 7 A
                jp NZ tryNext

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
                replicateM_ 3 $ call printZ1
                ld A [isLast]
                cp 0
                ret NZ
            pure ()


        printZ1 <- labelled mdo
            ld A [shiftState]
            cp 0
            ldVia A [shiftState] 0
            jp NZ shifted

            ld A [IX]
            inc IX
            sub 6
            jr C shift

            add A 0x41
            call 0x1fc
            ret

            shifted <- labelled do
                ld A [IX]
                inc IX
                -- TODO: it could be something other than space
                ld A 0x20
                call 0x1fc
                ret

            shift <- labelled do
                ldVia A [shiftState] 1
            ret

        text1 <- labelled $ db text1'
        buf <- labelled $ db [0, 0, 0]
        isLast <- labelled $ db [0]
        shiftState <- labelled $ db [0]

        pure ()
