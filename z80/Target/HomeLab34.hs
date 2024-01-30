{-# LANGUAGE RecordWildCards, NamedFieldPuns, RecursiveDo, BlockArguments #-}
{-# LANGUAGE BinaryLiterals, NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Target.HomeLab34 (game) where

import RatBC.Game
import RatBC.HomeLab34.Strip
import RatBC.HomeLab34.Binary
import RatBC.HomeLab34.Text

import Ratkai.Z80
import Target.HomeLab34.Video

import Z80
import Z80.Utils
import Z80.Machine.HomeLab.HL34 hiding (printA)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Bifunctor
import Control.Monad.Identity
import Control.Monad
import Data.Functor.Const
import System.FilePath
import Data.Word
import Data.Char (ord, toUpper, isLower)

release :: Bool
release = False

supportUndo :: Bool
supportUndo = False

supportQSave :: Bool
supportQSave = False

game :: Game Identity -> Z80ASM
game assets@Game{ minItem, maxItem, startRoom } = mdo
    -- di
    -- ld SP 0xbfff -- No turning back now, baybee!
    -- pageIO

    -- Restore output vector
    ld HL 0x0283
    ld [0x4004] HL

    -- Restore input vector
    ld HL 0x035c
    ld [0x4002] HL

    -- Run the game
    ldVia A [gameVars vars] 0x00
    routines <- gameLoop assetLocs platform vars

    clearScreen <- labelled do
        push IY
        ld IY 0x4000
        ld A 0x0c
        printCharA
        ld A 0x0d
        printCharA
        pop IY
        ret

    -- Input one line of text (up to 38 characters), store result in [HL]
    -- Mangles `HL`, `A` and `B`
    inputLine <- labelled $ mdo
        push IX
        push IY
        ld IY 0x4000

        forM_ "\b\b> " \c -> do
            ld A $ encodeChar c
            printCharA

        ld B 38
        ld IX lastChar
        withLabel \loop -> mdo
            ld [HL] 0xff

            -- Draw cursor
            ld A $ encodeChar '_'
            printCharA

            withLabel \waitKey -> do
                rst 0x18
                cp [IX]
                jp Z waitKey
                ld [IX] A
                Z80.or A
                jp Z waitKey

            -- Delete cursor
            push AF
            ld A $ encodeChar '\b'
            printCharA
            pop AF

            cp 0x0d -- End of line
            jr Z enter
            cp 0x07 -- Backspace
            jr Z backspace

            -- Normal character: print and record
            dec B
            jr Z noMoreRoom
            printCharA
            ld [HL] A
            inc HL
            jr loop

            noMoreRoom <- labelled do
                inc B -- So that next `dec B` will trigger `Z` again
                dec HL
                ld [HL] A
                ld A 0x07 -- Erase previous last character
                printCharA
                ld A [HL] -- Print new last character
                inc HL
                printCharA
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

                ld A $ encodeChar '\b'
                printCharA
                ld [HL] 0x00
                dec HL
                jr loop

            enter <- labelled do
                ld [HL] 0x20
                inc HL
                ld [HL] 0xff
                cr
                pop IY
                pop IX
                ret
            pure ()

        lastChar <- labelled $ db [0]
        pure ()

    readLine <- labelled do
        ld HL $ inputBuf vars
        call inputLine
        call paragraph

        parseLine_ assetLocs platform vars routines

    -- Match one word from `[HL]` vs. a dictionary entry at `[IX]`
    -- After: `A` contains the word code (or 0 on non-match), and
    -- `HL` is the rest of the input
    -- Clobbers: `BC`, `IY`
    matchWord <- labelled mdo
        -- Are we beyond the last word?
        ld A [IX]
        cp 0xff
        ld A 0
        ret Z

        push IX
        pop DE

        ld BC 5
        add IX BC
        -- Note: IX now points to the code of the word we're trying to match
        -- while DE points to the start of the word

        push IX
        push HL
        -- Match the first 5 characters of HL, or until there is a space
        decLoopB dictWordLength do
            ld A [DE]
            inc DE
            ld C [HL]
            cp C
            jp NZ noMatch
            cp $ encodeChar ' ' -- If next char to match is a space, then we're done
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
            Z80.xor A
            ret
        pure ()

    -- Input: IX contains the message bank, B is the message ID
    printMessageLn <- labelled do
        call printMessage
    paragraph <- labelled do
        cr
        ret

    -- Input: IX contains the message bank, B is the message ID
    printMessage <- labelled $ mdo
        ld D 0
        withLabel \next -> do
            -- Is this the message we want?
            dec B
            jr Z start

            ld E [IX]
            add IX DE
            jp next

        start <- labelled do
            ld B [IX]
            dec B

        withLabel \loop -> mdo
            inc IX
            ld A [IX]
            printCharA
            djnz loop
        cr
        ret

    printBCDPercentLn <- labelled $ when supportScore do -- XXX
        call 0x01a5
        ld A $ fromIntegral . ord $ '%'
        rst 0x28
        cr
        ret

    printChar <- labelled printCharA_
    let printCharA = call printChar
    let printString s = skippable \end -> mdo
            ld IY lbl
            decLoopB (fromIntegral $ length s) do
                ld A [IY]
                inc IY
                printCharA
            jr end
            lbl <- labelled $ db $ map encodeChar s
            pure ()

    let cr = do
            ld A $ fromIntegral . ord $ '\n'
            printCharA

    let platform = Platform{ clearScreen = call clearScreen, .. }
          where
            moveIsFinal = True -- TODO
            runMachineCode = False -- TODO
            printMessageListItem = printMessage
            space = 0x20
            newline = cr
            sleep = call 0x0f6
            setScreen = Nothing
            setTextColors = Nothing
            spriteOn = Nothing
            spriteOff = Nothing
            loadSaveGameVars = Nothing
            deathCallback = pure ()
            beforeParseError = pure ()

    assetLocs <- do
        let assets' = mapGameF (first BL.toStrict) . assemble . reflowMessages True 60 . preprocessGame $ assets
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

    dictBuf <- labelled $ resb 5

    vars <- do
        moved <- labelled $ db [0]
        meta <- labelled $ db [0]
        inputBuf <- labelled $ resb 40
        parseBuf <- labelled $ resb 5
        gameVars <- labelled $ resb 256
        let playerScore = gameVars + 0xfc
            playerHealth = gameVars + 0xfd
            playerStatus = gameVars + 0xfe
            playerLoc = gameVars + 0xff
        savedVars <- if supportQSave then fmap Just . labelled $ resb 256 else pure Nothing
        undoVars <- if supportUndo then do
            undoVars <- labelled $ resb 256
            undoCandidates <- labelled $ resb 256
            pure $ Just (undoVars, undoCandidates)
          else pure Nothing
        return Vars{..}

    unless release nop -- To see real memory usage instead of just image size
    pure ()
  where
    waitEnter = withLabel \loop -> do
        rst 0x18
        cp 0x0d
        jp NZ loop
