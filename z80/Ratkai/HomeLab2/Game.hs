{-# LANGUAGE RecordWildCards, NamedFieldPuns, RecursiveDo, BlockArguments #-}
{-# LANGUAGE BinaryLiterals, NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Ratkai.HomeLab2.Game (game) where

import RatBC.Game
import RatBC.HomeLab2.Strip
import RatBC.HomeLab2.Binary

import Ratkai.Z80
import Ratkai.HomeLab2.ZSCII

import Z80
import Z80.Utils
import HL2
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Bifunctor
import Control.Monad.Identity
import Control.Monad
import Data.Functor.Const
import System.FilePath
import Data.Word
import Data.Char (ord)

release :: Bool
release = True

supportUndo :: Bool
supportUndo = False

supportQSave :: Bool
supportQSave = True

game :: Game Identity -> Z80ASM
game assets@Game{ minItem, maxItem, startRoom } = mdo
    ld SP 0x7fff -- No turning back now, baybee!

    -- Restore input vector
    ldVia A [0x4002] 0x06
    ldVia A [0x4003] 0x03

    let dbgPrintA = unless release do
            push AF
            call 0x01a5
            pop AF

        videoOn = ld [0x3f00] A
        videoOff = ld [0x3e00] A

        printCharA = rst 0x28
        printString s = skippable \end -> mdo
            ld IY lbl
            decLoopB (fromIntegral $ length s) do
                ld A [IY]
                inc IY
                printCharA
            jr end
            lbl <- labelled $ db $ map (fromIntegral . ord) s
            pure ()

        waitEnter = do
            -- message1 13
            withLabel \loop -> do
                rst 0x18
                cp 0x0d
                jp NZ loop
            ret


    ldVia A [gameVars vars] 0x00

    call $ resetGameVars routines
    when supportQSave $ call $ qsave routines
    ldVia A [shiftState] 0

    routines <- gameLoop assetLocs platform vars

    clearScreen <- labelled do
        ld A 0x0c
        rst 0x28
        ld A 0x0d
        rst 0x28
        ret

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
        ld HL $ inputBuf vars
        call inputLine
        call paragraph
        jp parseLine

    parseLine <- labelled mdo
        -- For a bit of extra zoom-zoom
        videoOff
        call parseLine'
        videoOn
        ret

        parseLine' <- labelled $ parseLine_ assetLocs platform vars routines
        pure ()

    parseError <- labelled do
        videoOn
        message1 1
        jp readLine

    -- dbgPrintParseBuf <- labelled $ unless release do
    --     -- DEBUG: print parse buffer
    --     ld IY $ parseBuf vars
    --     replicateM_ 5 do
    --         ld A [IY]
    --         inc IY
    --         call 0x01a5
    --     ld A 0x0d
    --     rst 0x28
    --     ret

    -- Match one word from `[HL]` vs. a dictionary entry at `[IX]`
    -- After: `A` contains the word code (or 0 on non-match), and
    -- `HL` is the rest of the input
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
        -- Note: IX now points to the code of the word we're trying to match

        -- If dictBuf is empty, this is an invalid entry
        ld A [dictBuf]
        cp 0
        ret Z

        ld DE dictBuf
        push IX
        push HL
        -- Match the first 5 characters of HL, or until there is a space
        decLoopB 5 do
            ld A [DE]
            inc DE
            ld C [HL]
            cp C
            jp NZ noMatch
            cp 0x20 -- If next char to match is a space, then we're done
            jp Z match
            inc HL

        match <- labelled do
            -- Skip all remaining characters of the current word
            skippable \end -> loopForever do
                ld A [HL]
                cp 0x20
                jp Z end
                inc HL

            pop IX -- Discard pushed IX, since we want to "commit" our progress
            pop IX
            ld A [IX]
            ret

        noMatch <- labelled do
            pop HL
            pop IX
            inc IX
            ld A 0
            ret
        pure ()

    let message1 msg = do
            ld IX $ getConst . msgs1 $ assetLocs
            ld B msg
            call $ printMessage platform

    -- Input: IX contains the message bank, B is the message ID
    printlnZ <- labelled do
        call printZ
    paragraph <- labelled do
        ld A 0x0d
        rst 0x28
        rst 0x28
        ret

    -- Input: IX contains the message bank, B is the message ID
    printZ <- labelled $ withLabel \tryNext -> mdo
        -- Is this the message we want?
        dec B
        jr Z start

        loopForever do
            inc IX
            ld A [IX]
            inc IX
            bit 7 A
            jr NZ tryNext

        start <- labelled $ do
            ldVia A [shiftState] 0
            ldVia A [unpackIsLast] 0
            loopForever do
                call unpackZ
                ld HL unpackBuf
                replicateM_ 3 $ do
                    ld A [HL]
                    inc HL
                    call decodeZ1
                    skippable \unprintable -> do
                        jr Z unprintable
                        call 0x28
                ld A [unpackIsLast]
                cp 0
                ret NZ
        pure ()

    unpackZ <- labelled $ unpackZ_ unpackBuf unpackIsLast
    decodeZ1 <- labelled $ decodeZ1_ shiftState

    printBCDPercent <- labelled $ when supportScore do -- XXX
        call 0x01a5
        ld A $ fromIntegral . ord $ '%'
        rst 0x28
        ld A $ fromIntegral . ord $ '\r'
        rst 0x28
        ret

    let platform = Platform{ printMessage = printlnZ, clearScreen = call clearScreen, readLine = call readLine, .. }

    assetLocs <- do
        let assets' = mapGameF (first BL.toStrict) . assemble . reflowMessages 40 . preprocessGame $ assets
        let asset sel = labelled $ db $ getConst . sel $ assets'
        text1 <- asset msgs1
        text2 <- asset msgs2
        dict_ <- asset dict
        scriptEnter <- asset enterRoom
        scriptAfter <- asset afterTurn
        scriptGlobal <- asset interactiveGlobal
        scriptLocal <-  asset interactiveLocal
        help <- asset helpMap
        resetVars <- asset resetState
        resetData <- asset resetState
        return assets
            { msgs1 = Const text1
            , msgs2 = Const text2
            , dict = Const dict_
            , enterRoom = Const scriptEnter
            , afterTurn = Const scriptAfter
            , interactiveGlobal = Const scriptGlobal
            , interactiveLocal = Const scriptLocal
            , resetState = Const resetData
            , helpMap = Const help
            }

    unpackBuf <- labelled $ db [0, 0, 0]
    unpackIsLast <- labelled $ db [0]
    shiftState <- labelled $ db [0]
    dictBuf <- labelled $ resb 5

    -- let vars = Vars{..}
    vars <- do
        moved <- labelled $ db [0]
        inputBuf <- labelled $ resb 40
        parseBuf <- labelled $ resb 5
        gameVars <- labelled $ resb 256
        let playerScore = gameVars + 0xfc
            playerHealth = gameVars + 0xfd
            playerStatus = gameVars + 0xfe
            playerLoc = gameVars + 0xff
        savedVars <- if supportQSave then fmap Just . labelled $ resb 256 else pure Nothing
        undoVars <- if supportUndo then fmap Just . labelled $ resb 256 else pure Nothing
        return Vars{..}
    unless release nop -- To see real memory usage instead of just image size
    pure ()
