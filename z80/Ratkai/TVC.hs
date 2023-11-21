{-# LANGUAGE RecordWildCards, NamedFieldPuns, RecursiveDo, BlockArguments #-}
{-# LANGUAGE BinaryLiterals, NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Ratkai.TVC (game) where

import RatBC.Game
import RatBC.TVC.Binary
import RatBC.TVC.Text
import RatBC.TVC.Strip

import Ratkai.Z80
import TVC
import TVC.Keyboard
import TVC.Video as Video

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
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

release :: Bool
release = True

supportUndo :: Bool
supportUndo = False

supportQSave :: Bool
supportQSave = True

game :: Game Identity -> Z80ASM
game assets@Game{ minItem, maxItem, startRoom } = mdo
    let assets' = mapGameF (first BL.toStrict) . assemble . reflowMessages 31 . preprocessGame $ assets

    let printCharC = call printCharC4

    di

    -- Save current graphics settings
    ld A [0x0b13]

    -- Set video mode 4
    ld C 1
    syscall 4
    setInterruptHandler intHandler
    ei

    -- Set palette 1 (foreground) for text
    ld A 0b11_11_11_11
    out [0x61] A

    -- Set palette 2 (user input)
    ld A 0b00_00_00_00
    out [0x62] A

    -- Clear screen
    syscall 0x05

    -- Set border color
    ld A 0b00_00_10_00
    out [0x00] A

    -- ld HL picData
    ld A 0b00_11_11_00
    call displayPicture

    ldVia A [lineNum videoLocs] firstLine
    ldVia A [colNum videoLocs] 0

    -- Initialize state
    call $ resetGameVars routines
    when supportQSave $ call $ qsave routines

    -- Run the game
    let platform = Platform{..}
          where
            printString s = skippable \end -> mdo
                ld IY lbl
                decLoopB (fromIntegral $ length s) do
                    ld C [IY]
                    inc IY
                    printCharC
                jr end
                lbl <- labelled $ db $ map (fromIntegral . ord) s
                pure ()

            beforeParseError = pure ()
            waitEnter = pure ()
            clearScreen = pure ()
            space = tvcChar ' '

    call setMainColor
    routines <- gameLoop assetLocs platform vars

    pageVideoIn <- labelled pageVideoIn_
    pageVideoOut <- labelled pageVideoOut_
    displayPicture <- labelled $ displayPicture_ videoLocs
    intHandler <- labelled $ intHandler_ kbdBuf

    parseLine <- labelled $ parseLine_ assetLocs platform vars routines

    readLine <- labelled do
        ld HL $ inputBuf vars
        call inputLine
        call newLine
        -- forM_ [0..4] \i -> do
        --     ld A [inputBuf vars + i]
        --     call printByte
        call newLine
        jp parseLine

    -- Input one line of text, store result in `[HL]`
    -- Mangles `HL`, `A`, and `B`
    inputLine <- labelled do
        -- Set color for user input
        call setInputColor

        -- Draw prompt
        push HL
        ld C $ tvcChar '>'
        printCharC
        pop HL

        ld B maxInput
        withLabel \loop -> mdo
            ld [HL] 0xff

            push BC
            push HL

            push BC
            ld C $ tvcChar '_'
            printCharC
            call printBack
            pop BC

            withLabel \waitForInput -> do
                call readChar
                jp Z waitForInput
            pop HL
            pop BC

            cp $ tvcChar '\n' -- End of line
            jr Z enter
            cp $ tvcChar '\DEL' -- Backspace
            jr Z backspace

            -- Normal character: print and record
            dec B
            jr Z noMoreRoom

            ld [HL] A
            inc HL

            ld C A
            push BC
            push HL
            printCharC
            pop HL
            pop BC

            jr loop

            noMoreRoom <- labelled do
                inc B -- So that next `dec B` will trigger `Z` again
            --     dec HL
            --     ld [HL] A

            --     -- Erase previous character

            --     ld A 0x07 -- Erase previous last character
            --     rst 0x28
            --     ld A [HL] -- Print new last character
            --     inc HL
            --     rst 0x28
                jr loop

            backspace <- labelled do
                -- Try to increase B
                inc B
                skippable \inRange -> do
                    ld A B
                    cp (maxInput + 1)
                    jr NZ inRange
                    dec B
                    jr loop

                -- Replace last printed character with a space
                push HL
                push BC
                ld C $ tvcChar ' '
                printCharC
                replicateM_ 2 $ call printBack
                ld C $ tvcChar ' '
                printCharC
                call printBack
                pop BC
                pop HL

                ld [HL] 0x00
                dec HL
                jr loop

            enter <- labelled do
                ld [HL] 0x20
                inc HL
                ld [HL] 0xff

                -- Remove cursor
                ld C $ tvcChar ' '
                printCharC

                -- Restore color
                jp setMainColor
            pure ()

    readChar <- labelled $ mdo
        readChar_ keyData kbdBuf
        keyData <- labelled $ db $ toByteMap keymap
        pure ()

    printByte <- labelled $ printByte_ printCharC -- XXX

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
            cp $ tvcChar ' ' -- If next char to match is a space, then we're done
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

        -- noMoreWords <- labelled do
        --     ld A 0
        --     ret
        pure ()

    videoLocs <- do
        charset <- labelled $ db $ BL.toStrict . charSet $ assets'
        lineNum <- labelled $ db [0]
        colNum <- labelled $ db [0]
        drawColorIsInput <- labelled $ db [0]
        pure Video.Locations{..}

    printCharC4 <- labelled $ printCharC4_ videoLocs
    printBack <- labelled $ printBack_ videoLocs
    newLine <- labelled $ newLine_ videoLocs
    setMainColor <- labelled $ setMainColor_ videoLocs
    setInputColor <- labelled $ setInputColor_ videoLocs

    printBCDPercent <- labelled do
        ret -- TODO

    printMessage <- labelled mdo
        push BC
        push DE
        ld D 0
        withLabel \next -> do
            -- Is this the message we want?
            dec B
            jr Z start

            ld E [IX]
            add IX DE
            jp next

        start <- labelled $ do
            ld B [IX]
            dec B
        withLabel \loop -> mdo
            push BC
            inc IX
            ld A [IX]
            cp (tvcChar '\n')
            jp Z isNewLine

            ld C A
            push IX
            printCharC
            pop IX
            jr next

            isNewLine <- labelled $ call newLine

            next <- label
            pop BC
            djnz loop
        pop DE
        pop BC
        call newLine
        jp newLine

    -- picData <- labelled $ db pic
    kbdBuf <- labelled $ db $ replicate (fromIntegral kbdBufLen) 0xff

    assetLocs <- do
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

    vars <- do
        moved <- labelled $ db [0]
        inputBuf <- labelled $ resb (fromIntegral maxInput)
        parseBuf <- labelled $ resb 5
        gameVars <- labelled $ resb 256
        let playerScore = gameVars + 0xfc
            playerHealth = gameVars + 0xfd
            playerStatus = gameVars + 0xfe
            playerLoc = gameVars + 0xff
        savedVars <- if supportQSave then fmap Just . labelled $ resb 256 else pure Nothing
        undoVars <- if supportUndo then fmap Just . labelled $ resb 256 else pure Nothing
        return Vars{..}
    pure ()

toByteMap :: [(Word8, Word8)] -> BS.ByteString
toByteMap vals = BS.pack [ fromMaybe 0xff val | addr <- [0..255], let val = lookup addr vals ]

pageVideoIn_ :: Z80ASM
pageVideoIn_ = do
    ld A 0x50
    ld [0x03] A
    out [0x02] A
    ret

pageVideoOut_ :: Z80ASM
pageVideoOut_ = do
    ld A 0x70
    ld [0x03] A
    out [0x02] A
    ret

intHandler_ :: Location -> Z80ASM
intHandler_ kbdBuf = mdo
    push AF
    push BC
    push DE
    push HL
    push IX
    push IY
    out [0x07] A

    ld A [whichHalf]
    xor 0xff
    ld [whichHalf] A
    jp Z half2

    half1 <- labelled do
        when False do
            -- Set border color to dark green
            ld A 0b00_10_00_00
            out [0x00] A

        ld A [0x0b13]
        Z80.and 0b1111_1100
        Z80.or  0b0000_0010 -- Graphics mode 16
        out [0x06] A

        setupLineInt 87
        scanKeyboard kbdBuf
        jp finish

    repeatTimer <- labelled $ db [kbdRepeatRate]

    half2 <- labelled do
        when False do
            -- Set border color to red
            ld A 0b00_00_10_00
            out [0x00] A

        decLoopB 20 $ pure ()

        ld A [0x0b13]
        Z80.and 0b1111_1100
        Z80.or  0b0000_0001 -- Graphics mode 4
        -- Z80.or  0b0000_0000 -- Graphics mode 2
        out [0x06] A

        setupLineInt 239

        when False do
            -- Set border color to bright red
            ld A 0b10_00_10_00
            out [0x00] A

    finish <- label

    pop IY
    pop IX
    pop HL
    pop DE
    pop BC
    pop AF
    ei
    ret

    whichHalf <- labelled $ db [0]
    pure ()
  where
     setupLineInt y = do
         let (lo, hi) = wordBytes $ y * 16 {-+ 63-} -- - 46
         crtcOut 0x0e hi
         crtcOut 0x0f lo

toHex_ :: Z80ASM
toHex_ = mdo
    cp 10
    jp NC hex
    add A $ tvcChar '0'
    ret
    hex <- label
    add A $ tvcChar 'a' - 10
    ret

printByte_ :: Z80ASM -> Z80ASM
printByte_ printCharC = mdo
    push BC

    push AF
    Z80.and 0xf0
    replicateM_ 4 $ srl A
    call toHex

    ld C A
    printCharC

    pop AF
    Z80.and 0x0f
    call toHex

    ld C A
    printCharC

    pop BC
    ret

    toHex <- labelled toHex_
    pure ()
