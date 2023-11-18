{-# LANGUAGE RecordWildCards, NamedFieldPuns, RecursiveDo, BlockArguments #-}
{-# LANGUAGE BinaryLiterals, NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Ratkai.HomeLab2.Game (game) where

import RatBC.Game
import RatBC.HomeLab2.Strip
import RatBC.HomeLab2.Binary

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

supportHelp :: Bool
supportHelp = True

supportScore :: Bool
supportScore = True

supportUndo :: Bool
supportUndo = False

supportQSave :: Bool
supportQSave = True

moveIsFinal :: Bool
moveIsFinal = True

game :: Game Identity -> Z80ASM
game assets@Game{ minItem, maxItem, startRoom } = mdo
    let assets' = mapGameF (first BL.toStrict) . assemble . reflowMessages 40 . preprocessGame $ assets
    let asset sel = labelled $ db $ getConst . sel $ assets'
    let connective = 100 -- TODO

    let dbgPrintA = unless release do
            push AF
            call 0x01a5
            pop AF

    let videoOn = ld [0x3f00] A
        videoOff = ld [0x3e00] A

    ld SP 0x7fff -- No turning back now, baybee!

    -- Restore input vector
    ldVia A [0x4002] 0x06
    ldVia A [0x4003] 0x03

    ldVia A [gameVars] 0x00

    call resetGameVars
    -- when supportQSave $ call qsave -- XXX
    ldVia A [shiftState] 0

    -- Welcome message
    call clearScreen
    -- message1 14
    -- call waitEnter

    newGame <- label

    call clearScreen
    ldVia A [moved] 1

    withLabel \loop -> do
        skippable \notMoved -> do
            ld A [moved]
            dec A
            jp NZ notMoved
            ld [moved] A

            when False do -- Clear screen for each new location?
                ld A 0x0c
                rst 0x28
                ld A 0x0d
                replicateM_ 3 $ rst 0x28
            call runEnter

        -- check player status
        ld A [playerStatus]
        cp 0
        jp NZ gameOver

        call readLine
        -- call dbgPrintParseBuf

        ld A [parseBuf]
        cp 0x00
        jp Z loop

        skippable \processed -> do
            call runInteractiveLocal
            jp NZ processed
            call runInteractiveGlobal
            jp NZ processed
            call runInteractiveBuiltin
            jp Z processed

            message1 2
            jp loop

        -- check health
        skippable \healthy -> do
            ld A [playerHealth]
            cp 0
            jp NZ healthy
            ldVia A [playerStatus] 1

        -- call runAfter
        jp loop

    gameOver <- labelled do
        -- `A` contains the player's status:
        -- 1: Dead
        -- 2: Won the game
        skippable \notDead -> do
            cp 254
            jp Z notDead
            message1 0x0c
            when supportScore $ call printScore

        call resetGameVars
        ldVia A [gameVars + 1] 0xff

        call waitEnter
        jp newGame

    clearScreen <- labelled do
        ld A 0x0c
        rst 0x28
        ld A 0x0d
        rst 0x28
        ret

    waitEnter <- labelled do
        -- message1 13
        withLabel \loop -> do
            rst 0x18
            cp 0x0d
            jp NZ loop
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
        ld HL inputBuf
        call inputLine
        call paragraph
        jp parseLine

    parseLine <- labelled mdo
        -- For a bit of extra zoom-zoom
        videoOff
        call parseLine'
        videoOn
        ret

        parseLine' <- labelled $ parseLine_ assetLocs locations
        pure ()

    parseError <- labelled do
        videoOn
        message1 1
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
            ld IX text1
            ld B msg
            call printlnZ

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

    findByRoom <- labelled $ findByRoom_ locations
    findByWords <- labelled $ findByWords_ locations

    -- Are there any items at location `D`? If yes, set Z
    -- Clobbers `IY`, `A`, `B`
    anyItemsAtD <- labelled do
        ld IY $ gameVars + fromIntegral maxItem
        decLoopB (maxItem - minItem) $ skippable \notHere -> do
            dec IY
            ld A [IY]
            cp D
            ret Z
        ld A 0
        cp 1
        ret

    -- Print all items at location `D`.
    -- Clobbers `IY`, `A`, `B`
    printItemsAtD <- labelled do
        ld IY $ gameVars + fromIntegral maxItem
        decLoopB (maxItem - minItem) $ skippable \next -> do
            dec IY
            ld A [IY]
            cp D
            jp NZ next
            ld IX text2
            push BC
            ld A B
            add A $ minItem - 1
            ld B A
            call printlnZ
            pop BC
        ret

    -- Move all items at location `D` to location `E`. `C` is number of items moved.
    -- Clobbers `IY`, `A`, `B`
    moveItemsDE <- labelled do
        ld IY $ gameVars + fromIntegral maxItem
        ld C 0
        decLoopB (maxItem - minItem) $ skippable \next -> do
            dec IY
            ld A [IY]
            cp D
            jp NZ next
            inc C
            ld [IY] E
        ret

    let locations = Locations{ printMessage = printlnZ, .. }

    runEnter <- labelled $ runEnter_ assetLocs locations
    runAfter <- labelled $ runAfter_ assetLocs locations
    runInteractiveBuiltin <- labelled $ runInteractiveBuiltin_ assetLocs locations
    runInteractiveLocal <- labelled $ runInteractiveLocal_ assetLocs locations
    runInteractiveGlobal <- labelled $ runInteractiveGlobal_ assetLocs locations
    runInteractive <- labelled $ runInteractive_ assetLocs locations

    let fetch :: (Load r [HL]) => r -> Z80ASM
        fetch r = do
            ld r [HL]
            inc HL

    -- Run a Rat script starting at IX, with text bank HL
    runRatScript <- labelled $ runRatScript_ locations
    resetGameVars <- labelled $ resetGameVars_ assetLocs locations

    -- Pre: E is the variable's index
    -- Post: IY is the variable's address
    -- Clobbers: D, A, IY
    varIY <- labelled do
        ld IY gameVars
        ld D 0
        add IY DE
        ret

    getLocation <- labelled do
        ld A [playerLoc]
        ret

    printScore <- labelled $ printScore_ locations
    printBCDPercent <- labelled $ when supportScore do
        call 0x01a5
        ld A $ fromIntegral . ord $ '%'
        rst 0x28
        ld A $ fromIntegral . ord $ '\r'
        rst 0x28
        ret

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
    let assetLocs = assets
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

    moved <- labelled $ db [0]
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
    savedVars <- labelled $ when supportQSave $ resb 256
    undoVars <- labelled $ when supportUndo $ resb 256
    unless release nop -- To see real memory usage instead of just image size
    pure ()




data Locations = Locations
    { gameVars, moved :: Location
    , inputBuf :: Location
    , playerScore, playerHealth, playerStatus, playerLoc :: Location
    , varIY :: Location
    , printItemsAtD, anyItemsAtD :: Location
    , findByRoom, findByWords :: Location
    , runRatScript :: Location
    , runInteractive :: Location
    , printScore, printBCDPercent :: Location
    , savedVars, parseBuf :: Location
    , printMessage :: Location
    , matchWord :: Location
    , parseError :: Location
    }


-- | Run a Rat script starting at IX, with text bank HL
runRatScript_ :: Locations -> Z80ASM
runRatScript_ Locations{..} = mdo
    runRatScript <- labelled do
        fetch A
        cp 0x18
        jp C runRatStmt
    ratMessage <- labelled do
        ld B A
        push IX
        push HL
        call printMessage
        pop HL
        pop IX
        jp runRatScript

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
                    jp printMessage
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
            ld E [HL] -- Number of bytes to skip
            ld D 0
            add HL DE
            jp runRatScript

        let opIf val = do
                fetch E -- Variable to check
                call getVar
                cp val
                jp NZ opSkip -- Number of bytes to skip if check fails
                inc HL
                jp runRatScript
        opIf00 <- labelled $ opIf 0x00
        opIfFF <- labelled $ opIf 0xff

        opMoveTo <- labelled do
            fetch A
            ld [gameVars + 0xff] A
            ldVia A [moved] 1
            if moveIsFinal then ret else jp runRatScript

        opHeal <- labelled $ do
            ld IY playerHealth
            jp opAddToCounter
        opAddScore <- labelled $ do
            ld IY playerScore
            jp opAddToCounter
        opHurt <- labelled do
            ld IY playerHealth
            call opSubFromCounter

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
            jp printMessage

        opSleep <- labelled do
            fetch B
            when release do
                withLabel \loop -> do
                    exx
                    decLoopB 8 halt
                    exx
                    djnz loop
            jp runRatScript

        let unimplemented n = labelled do
                replicateM_ n $ inc IX
                jp runRatScript

            unsupported :: Z80 Location
            unsupported = pure 0x0000

        opSetScreen <- unsupported
        opSpriteOn <- unsupported
        opSpriteOff <- unsupported
        opChime <- unsupported
        opMachineCode <- unsupported
        opCopyProtection <- unsupported

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

        -- Add to counter in `[IY]`
        opAddToCounter <- labelled do
            ld A [IY]
            fetch B -- Value to add
            add A B
            daa
            skippable \noOverflow -> do
                jp NC noOverflow
                ld A 0x99
            ld [IY] A
            jp runRatScript

        -- Subtract from counter in `[IY]`
        opSubFromCounter <- labelled do
            ld A [IY]
            fetch B -- Value to add
            sub B
            daa
            skippable \noOverflow -> do
                jp NC noOverflow
                ld A 0
            ld [IY] A
            jp runRatScript
        pure ()
    pure ()
  where
    fetch :: (Load r [HL]) => r -> Z80ASM
    fetch r = do
        ld r [HL]
        inc HL

resetGameVars_ :: Game (Const Location) -> Locations -> Z80ASM
resetGameVars_ assets Locations{..} = mdo
    ld DE gameVars
    ld A 0x00
    decLoopB 256 do
        ld [DE] A
        inc DE

    ld DE $ gameVars + fromIntegral minItem
    ld HL resetState
    ld BC $ fromIntegral (maxItem - minItem)
    ldir

    ldVia A [playerLoc] startRoom
    ldVia A [playerHealth] 0x50 -- in BCD!
    ret

    pure ()
  where
    Game
      { minItem = minItem
      , maxItem = maxItem
      , resetState = Const resetState
      , startRoom = startRoom
      } = assets

-- | Returns NZ iff there was a matching command
runInteractiveLocal_ :: Game (Const Location) -> Locations -> Z80ASM
runInteractiveLocal_ assets Locations{..} = do
    ld HL $ getConst $ interactiveLocal assets
    call findByRoom
    inc HL
    jp runInteractive

-- | Returns NZ iff there was a matching command
runInteractiveGlobal_ :: Game (Const Location) -> Locations -> Z80ASM
runInteractiveGlobal_ assets Locations{..} = do
    ld HL $ getConst $ interactiveGlobal assets
    jp runInteractive

-- | Returns NZ iff there was a matching command
runInteractive_ :: Game (Const Location) -> Locations -> Z80ASM
runInteractive_ assets Locations{..} = do
    call findByWords
    ret Z

    ld IX $ getConst $ msgs1 assets
    call runRatScript
    -- Clear NZ flag
    ld A 0
    cp 1
    ret


-- | Returns Z iff there was a matching command
runInteractiveBuiltin_ :: Game (Const Location) -> Locations -> Z80ASM
runInteractiveBuiltin_ assets Locations{..} = mdo
    ld A [parseBuf]
    skippable \notMove -> do
        cp 0x0c
        jp NC notMove
        message1 3
        setZ
        ret

    let builtin val body = skippable \skip -> do
            cp val
            jp NZ skip
            body

    let finishWith :: (Load Reg8 msg) => Word16 -> msg -> Z80ASM
        finishWith bank msg = do
            ld IX bank
            ld B msg
            jp finish

    builtin 0x0d do -- Look
        ldVia A [moved] 1
        ret

    builtin 0x15 do -- Examine
        finishWith msgs1 9

    when supportScore do
        builtin 0x14 do -- Score
            jp printStatus

    when supportQSave do
        builtin 0x1b do         -- QSave
            call qsave
            finishWith msgs1 4

        builtin 0x1c do         -- QLoad
            ld DE gameVars
            ld HL savedVars
            ld BC 256
            ldir
            ldVia A [moved] 1
            finishWith msgs1 4

    when supportUndo do
        builtin 0x1a do -- Undo
            -- TODO
            finishWith msgs1 2

    builtin 0x16 do -- Help
        if not supportHelp then do
            finishWith msgs1 10
          else mdo
            ld D 0
            ldVia A E [playerLoc]
            ld HL help
            add HL DE
            ld A [HL]
            skippable \noHelp -> do
                cp 0
                jr Z noHelp
                finishWith msgs2 A
            finishWith msgs1 10

    builtin 0x10 mdo -- Inventory
        ld D 0
        call anyItemsAtD
        skippable \haveItems -> do
            jr Z haveItems
            finishWith msgs1 8
        message1 7
        call printItemsAtD
        setZ
        ret

    builtin 0x0e mdo -- Take
        ld A [parseBuf + 1]
        cp 0x00
        jp Z takeAll -- No nouns

        -- Is it an item?
        cp minItem
        jp C notItem
        cp maxItem
        jp NC notItem

        -- Is it here?
        ld E A
        call varIY
        ld A [playerLoc]
        cp [IY]
        jr NZ notHere

        -- Finally, all good
        ld [IY] 0
        finishWith msgs1 4

        notHere <- labelled do
            finishWith msgs1 5

        takeAll <- labelled mdo
            ldVia A D [playerLoc]
            ldVia A E 0x00
            call moveItemsDE

            -- Did we take anything after all?
            ld A C
            cp 0
            jr Z noItems
            finishWith msgs1 4

            noItems <- label
            finishWith msgs1 16

        finish <- label
        call printMessage
        setZ
        ret

    builtin 0x0f mdo -- Drop
        ld A [parseBuf + 1]
        cp 0x00
        jp Z dropAll -- No nouns

        -- Is it an item?
        cp minItem
        jp C notItem
        cp maxItem
        jp NC notItem

        -- Does the player have it?
        ld E A
        call varIY
        ld A 0
        cp [IY]
        jr NZ notHere

        -- Finally, all good
        ldVia A [IY] [playerLoc]
        finishWith msgs1 4

        notHere <- labelled do
            finishWith msgs1 6

        dropAll <- labelled mdo
            ldVia A D 0x00
            ldVia A E [playerLoc]
            call moveItemsDE

            -- Did we drop anything after all?
            ld A C
            cp 0
            jr Z noItems
            finishWith msgs1 4

            noItems <- label
            finishWith msgs1 8

        finish <- label
        call printMessage
        setZ
        ret

    ret

    notItem <- labelled do
        ld IX msgs1
        ld B 2
        -- Fall through to `finish`

    finish <- labelled do
        call printMessage
        setZ
        ret

    -- Move all items at location `D` to location `E`. `C` is number of items moved.
    -- Clobbers `IY`, `A`, `B`
    moveItemsDE <- labelled do
        ld IY $ gameVars + fromIntegral maxItem
        ld C 0
        decLoopB (maxItem - minItem) $ skippable \next -> do
            dec IY
            ld A [IY]
            cp D
            jp NZ next
            inc C
            ld [IY] E
        ret

    printStatus <- labelled $ when supportScore mdo
        ld IY lbl
        decLoopB (fromIntegral $ length s) do
            ld A [IY]
            inc IY
            rst 0x28
        ld A [playerHealth]
        call printBCDPercent
        jp printScore

        (lbl, s) <- labelledString "ERONLET:  "
        pure ()

    qsave <- labelled $ when supportQSave do
        ld DE savedVars
        ld HL gameVars
        ld BC 256
        ldir
        ret

    pure ()
  where
    Game
      { minItem = minItem
      , maxItem = maxItem
      , msgs1 = Const msgs1
      , msgs2 = Const msgs2
      , helpMap = Const help
      } = assets

    message1 msg = do
        ld IX msgs1
        ld B msg
        call printMessage

labelledString :: String -> Z80 (Location, String)
labelledString s = do
    l <- labelled $ db $ map (fromIntegral . ord) s
    pure (l, s)

printScore_ :: Locations -> Z80ASM
printScore_ Locations{..} = when supportScore mdo
    ld IY lbl
    decLoopB (fromIntegral $ length s) do
        ld A [IY]
        inc IY
        rst 0x28
    ld A [playerScore]
    call printBCDPercent
    setZ  -- So that the built-in handler for `SCORE` doesn't have to do this
    ret

    (lbl, s) <- labelledString "PONTSZAM: "
    pure ()

-- | Find data corresponding to the current room, starting at `HL`
--   Afterwards, HL points to the *length* of the current room's data,
--   and the actual data starts afterwards
findByRoom_ :: Locations -> Z80ASM
findByRoom_ Locations{..} = do
    ld A [playerLoc]
    ld B 0
    loopForever do
        dec A
        ret Z
        ld C [HL]
        add HL BC

-- Find data corresponding to the current input in parseBuf, starting at HL
-- Afterwards: Z iff no mach
findByWords_ :: Locations -> Z80ASM
findByWords_ Locations{..} = mdo
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

-- | Run "enter" script for current room
runEnter_ :: Game (Const Location) -> Locations -> Z80ASM
runEnter_ assets Locations{..} = do
    ld HL $ getConst . enterRoom $ assets
    call findByRoom
    inc HL
    ld IX $ getConst . msgs2 $ assets
    call runRatScript

    ldVia A D [playerLoc]
    call anyItemsAtD
    ret NZ

    ld IX $ getConst . msgs1 $ assets
    ld B 0x0b
    call printMessage
    jp printItemsAtD

runAfter_ :: Game (Const Location) -> Locations -> Z80ASM
runAfter_ assets Locations{..} = do
    ld HL $ getConst . afterTurn $ assets
    ld IX $ getConst . msgs1 $ assets
    jp runRatScript

parseLine_ :: Game (Const Location) -> Locations -> Z80ASM
parseLine_ assets Locations{..} = mdo
    -- -- For a bit of extra zoom-zoom
    -- videoOff

    -- Clear out `parseBuf`
    ld IY parseBuf
    ld A 0x00
    decLoopB 5 do
        ld [IY] A
        inc IY

    -- Parse up to 5 words from inputBuf into parseBuf
    ld HL inputBuf
    ld IY parseBuf
    skippable \end -> decLoopB 5 $ withLabel \doesntCount -> do
        -- Skip all leading spaces
        ld A 0x20
        skippable \end -> loopForever do
            cp [HL]
            jp NZ end
            inc HL

        ld A [HL]
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
    -- videoOn
    ret

    -- | Parse one word from [HL] into [IY], advancing `HL` as needed
    --   Post: flag Z iff parse error
    parse1 <- labelled mdo
        ld IX $ getConst . dict $ assets
        loopForever do
            ld A [IX]
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
    pure ()
  where
    connective = 100 -- TODO
