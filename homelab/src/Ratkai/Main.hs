{-# LANGUAGE RecordWildCards, RecursiveDo, BlockArguments #-}
{-# LANGUAGE BinaryLiterals, NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
module Ratkai.Main (game) where

import Z80
import Z80.Utils
import HL2
import qualified Data.ByteString as BS
import Control.Monad
import System.FilePath
import Data.Word

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
    let connective = 100 -- TODO
    let minItem = 120 -- TODO
    let maxItem = 160 -- TODO
    let startRoom = 1 -- TODO

    let dbgPrintA = do
            push AF
            call 0x01a5
            pop AF

    let videoOn = ld [0x3f00] A
        videoOff = ld [0x3e00] A

    pure $ mdo
        call resetGameVars

        -- Clear screen
        ld A 0x0c
        rst 0x28

        ldVia A [shiftState] 0

        call runEnter

        loopForever $ withLabel \loop -> do
            call readLine
            skippable \noGlobal -> do
                ld A [parseBuf]
                cp 0x00
                jp Z noGlobal

                ld HL scriptGlobal
                call findByWords
                jp Z noGlobal

                push HL
                pop IX
                ld HL text1
                call runRatScript

                jp loop

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
                ld [HL] 0xff
                rst 0x18
                cp 0x0d -- End of line
                jr Z enter
                cp 0x07 -- Backspace
                jr Z backspace

                -- Normal character: print and record
                dec B
                jr Z noMoreRoom
                rst 0x28
                ld [HL] A
                inc HL
                jr loop

                noMoreRoom <- labelled do
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
                    -- Try to increase B
                    inc B
                    skippable \inRange -> do
                        ld A B
                        cp 39
                        jr NZ inRange
                        dec B
                        jr loop

                    ld A 0x07
                    rst 0x28
                    ld [HL] 0x00
                    dec HL
                    jr loop

                enter <- labelled do
                    ld [HL] 0x20
                    inc HL
                    ld [HL] 0xff
                    ret
                pure ()

        readLine <- labelled do
            ld HL inputBuf
            call inputLine
            call paragraph
            jp parseLine

        parseLine <- labelled do
            -- For a bit of extra zoom-zoom
            videoOff

            -- Clear out `parseBuf`
            ld IY parseBuf
            decLoopB 5 do
                ldVia A [IY] 0x00
                inc IY

            -- Parse up to 5 words from inputBuf into parseBuf
            ld IY parseBuf
            ld IX inputBuf
            skippable \end -> decLoopB 5 $ withLabel \doesntCount -> do
                -- Skip all leading spaces
                skippable \end -> loopForever do
                    ld A [IX]
                    cp 0x20
                    jp NZ end
                    inc IX

                ld A [IX]
                cp 0xff
                jr Z end
                push BC
                call parse1
                pop BC
                jr Z parseError

                -- Is this a connective? If so, it doesn't count
                skippable \notConnective -> do
                    ld A [IY]
                    cp connective
                    jp NZ notConnective
                    ld [IY] 0x00
                    jp doesntCount
                inc IY
            videoOn
            ret

        parseError <- labelled do
            videoOn
            ld HL text1
            ld B 1
            call printlnZ
            jp readLine

        dbgPrintParseBuf <- labelled do
            -- DEBUG: print parse buffer
            ld IY parseBuf
            replicateM_ 5 do
                ld A [IY]
                inc IY
                call 0x01a5
            ld A 0x0d
            rst 0x28
            ret

        -- Parse one word from [IX] into [IY], advancing `IX` as needed
        -- Post: flag Z iff parse error
        parse1 <- labelled mdo
            ld HL dict
            loopForever do
                ld A [HL]
                cp 0xff
                ret Z

                push IY
                call matchWord
                pop IY
                cp 0x00
                jp NZ found

            found <- label
            ld [IY] A
            ret

        -- Match one word from `[IX]` vs. a dictionary entry at `[HL]`
        -- After: `A` contains the word code (or 0 on non-match), and
        -- `IX` is the rest of the input
        -- Clobbers: `BC`, `IY`
        matchWord <- labelled mdo
            ldVia A [shiftState] 0
            ldVia A [unpackIsLast] 0
            -- Unpack word into dictBuf
            ld DE dictBuf
            ldVia A [dictBuf] 0x00
            withLabel \keepDecoding -> do
                call unpackZ
                ld IY unpackBuf
                replicateM_ 3 $ do
                    ld A [IY]
                    inc IY
                    call decodeZ1
                    skippable \unprintable -> do
                        jp Z unprintable
                        ld [DE] A
                        inc DE
                ld A [unpackIsLast]
                cp 0
                jp Z keepDecoding
            -- Note: HL now points to the code of the word we're trying to match

            -- If dictBuf is empty, this is an invalid entry
            ld A [dictBuf]
            cp 0
            ret Z

            ld DE dictBuf
            push HL
            push IX
            -- Match the first 5 characters of HL, or until there is a space
            decLoopB 5 do
                ld A [DE]
                inc DE
                ld C [IX]
                cp C
                jp NZ noMatch
                cp 0x20 -- If next char to match is a space, then we're done
                jp Z match
                inc IX

            match <- labelled do
                -- Skip all remaining characters of the current word
                skippable \end -> loopForever do
                    ld A [IX]
                    cp 0x20
                    jp Z end
                    inc IX

                pop HL -- Discard pushed IX, since we want to "commit" our progress
                pop HL
                ld A [HL]
                ret

            noMatch <- labelled do
                pop IX
                pop HL
                inc HL
                ld A 0
                ret
            pure ()

        printlnZ <- labelled do
            call printZ
        paragraph <- labelled do
            ld A 0x0d
            rst 0x28
            rst 0x28
            ret

        -- Input: HL contains the message bank, B is the message ID
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

            start <- labelled $ do
                ldVia A [shiftState] 0
                ldVia A [unpackIsLast] 0
                loopForever do
                    call unpackZ
                    ld IX unpackBuf
                    replicateM_ 3 $ do
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
            push DE
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

            pop DE
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

        -- Find data corresponding to the current room, starting at IX
        -- Afterwards, IX points to the *length* of the current room's data,
        -- and the actual data starts afterwards
        findByRoom <- labelled do
            ld A [gameVars + 0xff]
            ld B 0
            loopForever do
                dec A
                ret Z
                ld C [IX]
                add IX BC

        -- Find data corresponding to the current input in parseBuf, starting at HL
        -- Afterwards: Z iff no mach
        findByWords <- labelled mdo
            ld D 0 -- We'll use DE as an offset to add to HL

            loopForever mdo
                ld [savedHL] HL

                ld A [HL] -- Load skip amount, needed later if no match
                inc HL

                -- No more entries?
                cp 0
                ret Z
                ld E A

                -- Compare with parseBuf
                ld IX parseBuf
                loopForever do
                    ld B [IX] -- The next input token
                    inc IX
                    ld A [HL] -- The pattern from the set of responses
                    inc HL

                    cp B -- Are they the same word?
                    jp NZ noMatch

                    cp 0 -- If this is the last word, we're done
                    jp Z match

                match <- labelled do
                    -- Clear Z flag
                    ld A 1
                    cp 0
                    ret

                noMatch <- label
                -- No match, so skip (based on original origin!)
                ld HL [savedHL]
                add HL DE

            savedHL <- labelled $ dw [0]
            pure ()

        -- Run "enter" script for current room
        runEnter <- labelled do
            ld IX scriptEnter
            call findByRoom
            inc IX
            ld HL text2
            jp runRatScript

        runAfter <- labelled do
            ld IX scriptAfter
            ld HL text1
            jp runRatScript

        runInteractiveGlobal <- labelled do
            pure ()

        let fetch :: (Load r [RegIx]) => r -> Z80ASM
            fetch r = do
                ld r [IX]
                inc IX

        -- Run a Rat script starting at IX, with text bank HL
        runRatScript <- labelled do
            fetch A
            cp 0x18
            jp C runRatStmt
        ratMessage <- labelled do
            ld B A
            push IX
            push HL
            call printlnZ
            pop HL
            pop IX
            jp runRatScript

        runRatStmt <- labelled mdo
            -- We know `A` is at most 0x18, i.e. a valid stmt opcode
            -- dbgPrintA

            -- Compute jump table address into DE
            ld DE opTable
            sla A
            add A E
            ld E A
            skippable \noCarry -> do
                jp NC noCarry
                inc D

            -- Load jump destination
            ldVia A [trampoline + 1] [DE]
            inc DE
            ldVia A [trampoline + 2] [DE]

            -- Do the jump
            trampoline <- labelled do
                jp 0x0000

            opRet <- labelled do
                ret

            opMessage <- labelled do
                fetch A -- Message to print
                jp ratMessage

            let opAssert val = do
                    fetch E -- Variable to check
                    fetch B -- Message to print if assertion fails
                    call getVar
                    skippable \assertHolds -> do
                        cp val
                        jp Z assertHolds
                        jp printlnZ
                    jp runRatScript
            opAssert00 <- labelled $ opAssert 0x00
            opAssertFF <- labelled $ opAssert 0xff

            opAssign <- labelled do
                fetch E -- Variable
                fetch A -- Value
                call putVar
                jp runRatScript

            opSetPlayerStatus <- labelled do
                fetch A -- Value
                ld [playerStatus] A
                jp runRatScript

            let opAssignConst val = do
                    fetch E -- Variable
                    ld A val
                    call putVar
                    jp runRatScript
            opAssignFF <- labelled $ opAssignConst 0xff
            opAssign00 <- labelled $ opAssignConst 0x00

            opAssignLoc <- labelled do
                fetch E -- Variable
                ld A [gameVars + 0xff] -- Value
                call putVar
                jp runRatScript

            opSkip <- labelled do
                ld E [IX] -- Number of bytes to skip
                ld D 0
                add IX DE
                jp runRatScript

            let opIf val = do
                    fetch E -- Variable to check
                    call getVar
                    cp val
                    jp NZ opSkip -- Number of bytes to skip if check fails
                    inc IX
                    jp runRatScript
            opIf00 <- labelled $ opIf 0x00
            opIfFF <- labelled $ opIf 0xff

            opMoveTo <- labelled do
                fetch A
                ld [gameVars + 0xff] A
                jp runEnter

            let opAddToCounter var = do
                    ldVia A B [var]
                    fetch A -- Value to add
                    add A B
                    skippable \noOverflow -> do
                        cp 99
                        jp C noOverflow
                        ld A 99
                    ld [var] A
                    jp runRatScript

            opHeal <- labelled $ opAddToCounter playerHealth
            opAddScore <- labelled $ opAddToCounter playerScore

            opHurt <- labelled do
                ldVia A B [playerHealth]
                fetch A -- Value to subtract
                skippable \noUnderflow -> do
                    cp B
                    jp NC noUnderflow
                    ldVia A [playerHealth] 0
                    jp runRatScript
                sub B
                ld [playerHealth] A
                jp runRatScript

            opIncIfNot0 <- labelled do
                fetch E -- Variable
                call getVar
                cp 0
                jp Z runRatScript
                inc A
                ld [IY] A
                jp runRatScript

            opAssertHere <- labelled do
                fetch E -- Item to check
                fetch B -- Message to print when item is not here
                call getVar
                cp 0x00   -- Item is in the player's posession
                jp Z runRatScript
                ld C A
                ld A [playerLoc]
                cp C
                jp Z runRatScript
                jp printlnZ

            let unimplemented n = labelled do
                    replicateM_ n $ inc IX
                    jp runRatScript
                unsupported n = pure 0x0000

            opSetScreen <- unsupported 3
            opSpriteOn <- unsupported 5
            opSpriteOff <- unsupported 1
            opChime <- unsupported 1
            opSleep <- unimplemented 1
            opMachineCode <- unimplemented 1 -- XXX
            opCopyProtection <- unsupported 4

            opTable <- labelled $ dw
                [ opRet             -- 00
                , opAssign          -- 01
                , opMessage         -- 02
                , opAssign00        -- 03
                , opAssignFF        -- 04
                , opAssignLoc       -- 05
                , opAssert00        -- 06
                , opAssertFF        -- 07
                , opAssertHere      -- 08
                , opSkip            -- 09
                , opIf00            -- 0a
                , opIfFF            -- 0b
                , opMoveTo          -- 0c
                , opSetPlayerStatus -- 0d
                , opHeal            -- 0e
                , opHurt            -- 0f
                , opAddScore        -- 10
                , opSetScreen       -- 11
                , opSpriteOn        -- 12
                , opSpriteOff       -- 13
                , opChime           -- 14
                , opSleep           -- 15
                , opIncIfNot0       -- 16
                , opMachineCode     -- 17
                , opCopyProtection  -- 18
                ]
            pure ()


        resetGameVars <- labelled do
            ld DE gameVars
            decLoopB minItem do
                ldVia A [DE] 0x00
                inc DE
            ld HL resetVars
            ld BC $ fromIntegral $ (maxItem - minItem + 1)
            ldir
            decLoopB (maxBound - maxItem) do
                ldVia A [DE] 0x00
                inc DE
            ldVia A [playerLoc] startRoom
            ldVia A [playerHealth] 50
            ret

        -- Pre: E is the variable's index
        -- Post: IY is the variable's address
        -- Clobbers: D, A, IY
        varIY <- labelled do
            ld IY gameVars
            ld D 0
            add IY DE
            ret

        -- Pre: E is the variable's index
        -- Post: A is the variable's value
        -- Clobbers: D, IY
        getVar <- labelled do
            call varIY
            ld A [IY]
            ret

        -- Pre: E is the variable's index, A is its value-to-be
        -- Clobbers: D, IY
        putVar <- labelled do
            call varIY
            ld [IY] A
            ret

        getLocation <- labelled do
            ld A [playerLoc]
            ret

        text1 <- labelled $ db text1'
        text2 <- labelled $ db text2'
        dict <- labelled $ db dict'
        scriptEnter <- labelled $ db scriptEnter'
        scriptAfter <- labelled $ db scriptAfter'
        scriptGlobal <- labelled $ db scriptGlobal'
        scriptLocal <-  labelled $ db scriptLocal'
        help <- labelled $ db help'
        resetVars <- labelled $ db reset'

        unpackBuf <- labelled $ db [0, 0, 0]
        unpackIsLast <- labelled $ db [0]
        shiftState <- labelled $ db [0]
        inputBuf <- labelled $ resb 40
        parseBuf <- labelled $ resb 5
        dictBuf <- labelled $ resb 5
        gameVars <- labelled $ resb 256
        let playerScore = gameVars + 0xfc
            playerHealth = gameVars + 0xfd
            playerStatus = gameVars + 0xfe
            playerLoc = gameVars + 0xff
        pure ()
