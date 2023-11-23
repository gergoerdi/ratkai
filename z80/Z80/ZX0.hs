{-# LANGUAGE RecursiveDo, BlockArguments #-}
module Z80.ZX0 where

-- Port of https://github.com/einar-saukas/ZX0/ for easy inclusion in
-- Haskell Z80 projects

import Z80
import Z80.Utils

-- | `HL`: last source address
--   `DE`: last destination address
standardBack :: Z80ASM
standardBack = mdo
    ld BC 1    -- preserve default offset
    push BC
    ld A 0x80

    literals <- labelled do
        call elias -- obtain length
        lddr       -- copy literals
        inc C
        add A A    -- copy from last offset or new offset?
        jr C newOffset
        call elias

    copy <- labelled do
        ex [SP] HL  -- preserve source, restore offset
        push HL     -- preserve offset
        add HL DE   -- calculate destination - offset
        lddr        -- copy from offset
        inc C
        pop HL      -- restore offset
        ex [SP] HL  -- preserve offset, restore source
        add A A     -- copy from literals or new offset?
        jr NC literals

    newOffset <- labelled do
        inc SP      -- discard last offset
        inc SP
        call elias  -- obtain offset MSB
        dec B
        ret Z       -- check end marker
        dec C       -- adjust for positive offset
        ld B C
        ld C [HL]   -- obtian offset LSB
        dec HL
        srl B       -- last offset bit becomes first length bit
        rr C
        inc BC
        push BC     -- preserve new offset
        ld BC 1     -- obtain length
        call C eliasBacktrack
        inc BC
        jr copy

    eliasBacktrack <- labelled do
        add A A
        rl C
        rl B

    elias <- labelled do
        skippable \skip -> do
            add A A   -- inverted nterlaced Elias gamma coding
            jr NZ skip
            ld A [HL] -- load another group of 8 bits
            dec HL
            rla
        jr C eliasBacktrack
    ret
