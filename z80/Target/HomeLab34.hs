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

import Z80
import Z80.Utils
import Z80.Machine.HomeLab.HL34
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
        printA
        ld A 0x0d
        printA
        pop IY
        ret

    -- Input one line of text (up to 38 characters), store result in [HL]
    -- Mangles `HL`, `A` and `B`
    inputLine <- labelled $ mdo
        push IY
        ld IY 0x4000

        ld A $ encodeChar '>'
        printA
        ld A $ encodeChar ' '
        printA
        flushOut

        push HL
        call 0x0546
        cr
        pop HL

        skippable \end -> decLoopB 38 do
            rst 0x10
            Z80.or A
            jp Z end
            ld [HL] A
            inc HL
        ld [HL] $ encodeChar ' '
        inc HL
        ld [HL] 0xff

        pop IY
        ret

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
        push IY
        ld IY 0x4000
        cr
        pop IY
        ret

    -- Input: IX contains the message bank, B is the message ID
    printMessage <- labelled $ mdo
        push IY
        ld IY 0x4000

        ld D 0
        withLabel \next -> do
            -- Is this the message we want?
            dec B
            jr Z start

            ld E [IX]
            add IX DE
            jp next

        start <- labelled do
            ld E [IX]
            inc IX
            dec E

        push IX
        pop HL
        call 0x1538

        -- start <- labelled $ do
        --     ld B [IX]
        --     dec B

        -- withLabel \loop -> mdo
        --     inc IX
        --     ld A [IX]
        --     cp (encodeChar '\n')
        --     jp Z isNewLine
        --     printA
        --     jr next

        --     isNewLine <- labelled cr

        --     next <- label
        --     djnz loop
        cr

        pop IY
        ret

    printBCDPercentLn <- labelled $ when supportScore do -- XXX
        call 0x01a5
        ld A $ fromIntegral . ord $ '%'
        rst 0x28
        cr
        ret

    let cr = do
            ld A $ fromIntegral . ord $ '\r'
            printA
        flushOut = do
            Z80.xor A
            printA

    let platform = Platform{ clearScreen = call clearScreen, .. }
          where
            moveIsFinal = True -- TODO
            runMachineCode = False -- TODO
            printMessageListItem = printMessage
            space = 0x20
            newline = do
                push IY
                ld IY 0x4000
                cr
                pop IY
            setScreen = Nothing
            setTextColors = Nothing
            spriteOn = Nothing
            spriteOff = Nothing
            loadSaveGameVars = Nothing
            deathCallback = pure ()
            beforeParseError = pure ()

    assetLocs <- do
        let assets' = mapGameF (first BL.toStrict) . assemble . {- reflowMessages 64 . -} preprocessGame $ assets
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
    printCharA = rst 0x28
    printString s = skippable \end -> mdo
        ld IY lbl
        decLoopB (fromIntegral $ length s) do
            ld A [IY]
            inc IY
            printCharA
        jr end
        lbl <- labelled $ db $ map toHL2 s
        pure ()
      where
        toHL2 c
          | isLower c = toHL2 (toUpper c)
          | c == 'Á' = toHL2 'A'
          | c == 'É' = toHL2 'E'
          | c == 'Í' = toHL2 'I'
          | c == 'Ó' = toHL2 'O'
          | c == 'Ú' = toHL2 'U'
          | c == 'Ö' = toHL2 'O'
          | c == 'Ő' = toHL2 'O'
          | c == 'Ü' = toHL2 'U'
          | c == 'Ű' = toHL2 'U'
          | otherwise = fromIntegral . ord $ c

    waitEnter = withLabel \loop -> do
        rst 0x18
        cp 0x0d
        jp NZ loop
