module Target.HomeLab34.Video where

import RatBC.HomeLab34.Text

import Z80
import Z80.Utils
import Z80.Machine.HomeLab.HL34 hiding (printA)

-- | Print the character stored in `A`. Supports `\n` for newline,
--   `\b` for backspace, and `\r` for clearing the screen.
printCharA_ :: Z80ASM
printCharA_ = mdo
    pageIO
    push HL

    push AF
    ld A [ptr + 1]
    cp $ snd $ wordBytes $ videoStart
    call C scrollUp
    pop AF

    cp $ encodeChar '\r'
    jp Z clearScreen

    cp $ encodeChar '\n'
    jp Z newline

    cp $ encodeChar '\b'
    jp Z backspace

    ld HL [ptr]
    ld [HL] A
    inc HL
    ld [ptr] HL

    finish <- labelled do
        pop HL
        pageRAM
        ret

    newline <- labelled do
        ld HL [ptr]
        push DE
        ld DE 64
        add HL DE
        pop DE
        ld A L
        Z80.and 0b1100_0000
        inc A
        inc A
        ld L A
        ld [ptr] HL
    jp finish

    clearScreen <- labelled do
        ld HL (videoStart + 2)
        ld [ptr] HL
        dec HL
        dec HL
        ld A 0x00
        push BC
        decLoopB 8 do
            ld C B
            decLoopB 256 do
                ld [HL] A
                inc HL
            ld B C
        pop BC
    jp finish

    backspace <- labelled do
        ld HL [ptr]
        dec HL
        ld [ptr] HL
        ld [HL] 0x00
    jp finish

    scrollUp <- labelled do
        push BC
        push DE
        ld HL $ videoStart + rowstride
        ld DE videoStart
        ld BC $ videoSize - rowstride
        ldir

        ld HL $ videoStart + videoSize - rowstride
        decLoopB rowstride do
            ld [HL] 0x00
            inc HL

        ld DE $ negate rowstride
        ld HL [ptr]
        add HL DE
        ld [ptr] HL
        pop DE
        pop BC
        ret

    ptr <- labelled $ dw [videoStart + 2]
    pure ()
