{-# LANGUAGE RecordWildCards, NamedFieldPuns, RecursiveDo, BlockArguments #-}
{-# LANGUAGE BinaryLiterals, NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
module Ratkai.Z80 where

import RatBC.Game

import Z80
import Z80.Utils
import qualified Data.ByteString as BS
import Control.Monad
import Data.Functor.Const
import Data.Word

supportSleep :: Bool
supportSleep = True

supportHelp :: Bool
supportHelp = True

supportScore :: Bool
supportScore = True

supportSound :: Bool
supportSound = True

supportGraphics :: Bool
supportGraphics = True

dictWordLength :: Word8
dictWordLength = 5

maxOpCode :: Word8
maxOpCode = 0x30

data Platform = Platform
    { printMessage :: Location
    , matchWord :: Location
    , beforeParseError :: Z80ASM
    , printString :: String -> Z80ASM
    , waitEnter :: Z80ASM
    , clearScreen :: Z80ASM
    , readLine :: Location
    , printlnBCDPercent :: Location
    , space :: Word8
    , newline :: Z80ASM
    , setScreen :: Maybe Z80ASM
    , setTextColors :: Maybe Z80ASM
    , spriteOn :: Maybe Z80ASM
    , spriteOff :: Maybe Z80ASM
    , moveIsFinal :: Bool
    , runMachineCode :: Bool
    }

data Vars = Vars
    { moved
    , inputBuf, parseBuf
    , gameVars, playerScore, playerHealth, playerStatus, playerLoc :: Location
    , savedVars, undoVars :: Maybe Location
    }

data Routines = Routines
    { varIY :: Location
    , printItemsAtD, anyItemsAtD, moveItemsDE :: Location
    , findByRoom, findByWords :: Location
    , runRatScript :: Location
    , runInteractive :: Location
    , printScore :: Location
    , qsave :: Location
    }


-- | Run a Rat script starting at IX, with text bank HL
runRatScript_ :: Platform -> Vars -> Routines -> Z80ASM
runRatScript_ Platform{..} Vars{..} Routines{..} = mdo
    runRatScript <- labelled do
        fetch A
        cp maxOpCode
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
            when supportSleep do
                withLabel \loop -> do
                    exx
                    decLoopB 8 halt
                    exx
                    djnz loop
            jp runRatScript

        let unimplemented n = labelled do
                replicateM_ n $ inc HL
                jp runRatScript

            unsupported :: Z80 Location
            unsupported = pure 0x0000

        opSetScreen <- case setScreen of
            Nothing -> unsupported
            Just setScreen -> labelled do
                fetch A -- Border color
                fetch B -- Background color
                fetch C -- Picture number
                setScreen
                jp runRatScript

        opSpriteOn <- case spriteOn of
            Nothing -> unsupported
            Just spriteOn -> labelled $ do
                fetch A -- Sprite #
                fetch B -- Sprite address
                fetch C -- Sprite color
                fetch D -- X coordinate
                fetch E -- Y coordinate
                spriteOn
                jp runRatScript

        opSpriteOff <- case spriteOff of
            Nothing -> unsupported
            Just spriteOff -> labelled $ do
                fetch A -- Sprite #
                spriteOff
                jp runRatScript

        opChime <- if supportSound then unimplemented 1 else unsupported

        opSetTextColors <- case setTextColors of
            Nothing -> unsupported
            Just setTextColors -> labelled do
                fetch A -- Output text color
                fetch B -- Input text color
                setTextColors
                jp runRatScript

        opMachineCode <- if not runMachineCode then unsupported else labelled do
            ldVia A D 0x00
            ldVia A E 95
            call moveItemsDE
            jp runRatScript

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
            , 0x0000            -- 18
            , opSetTextColors   -- 19
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

resetGameVars_ :: Game (Const Location) -> Vars -> Routines -> Z80ASM
resetGameVars_ assets Vars{..} Routines{..} = mdo
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
runInteractiveLocal_ :: Game (Const Location) -> Routines -> Z80ASM
runInteractiveLocal_ assets Routines{..} = do
    ld HL $ getConst $ interactiveLocal assets
    call findByRoom
    inc HL
    jp runInteractive

-- | Returns NZ iff there was a matching command
runInteractiveGlobal_ :: Game (Const Location) -> Routines -> Z80ASM
runInteractiveGlobal_ assets Routines{..} = do
    ld HL $ getConst $ interactiveGlobal assets
    jp runInteractive

-- | Returns NZ iff there was a matching command
runInteractive_ :: Game (Const Location) -> Routines -> Z80ASM
runInteractive_ assets Routines{..} = do
    call findByWords
    ret Z

    ld IX $ getConst $ msgs1 assets
    call runRatScript
    -- Clear NZ flag
    ld A 0
    cp 1
    ret


-- | Returns Z iff there was a matching command
runInteractiveBuiltin_ :: Game (Const Location) -> Platform -> Vars -> Routines -> Z80ASM
runInteractiveBuiltin_ assets Platform{..} Vars{..} Routines{..} = mdo
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

    forM_ savedVars \savedVars -> do
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

    forM_ undoVars \undoVars -> do
        builtin 0x1a do -- Undo
            -- TODO
            finishWith msgs1 2

    builtin 0x16 do -- Help
        if not supportHelp then do
            finishWith msgs1 10
          else mdo
            ld D 0
            ldVia A E [playerLoc]
            dec E
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

    printStatus <- labelled $ when supportScore mdo
        printString "Erőnlét:  "
        ld A [playerHealth]
        call printlnBCDPercent
        jp printScore

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

printScore_ :: Platform -> Vars -> Routines -> Z80ASM
printScore_ Platform{..} Vars{..} Routines{..} = when supportScore mdo
    printString "Pontszám: "
    ld A [playerScore]
    call printlnBCDPercent
    newline
    setZ  -- So that the built-in handler for `SCORE` doesn't have to do this
    ret

-- | Find data corresponding to the current room, starting at `HL`
--   Afterwards, HL points to the *length* of the current room's data,
--   and the actual data starts afterwards
findByRoom_ :: Vars -> Routines -> Z80ASM
findByRoom_ Vars{..} Routines{..} = do
    ld A [playerLoc]
    ld B 0
    loopForever do
        dec A
        ret Z
        ld C [HL]
        add HL BC

-- Find data corresponding to the current input in parseBuf, starting at HL
-- Afterwards: Z iff no mach
findByWords_ :: Vars -> Routines -> Z80ASM
findByWords_ Vars{..} Routines{..} = mdo
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
runEnter_ :: Game (Const Location) -> Platform -> Vars -> Routines -> Z80ASM
runEnter_ assets Platform{..} Vars{..} Routines{..} = do
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

runAfter_ :: Game (Const Location) -> Routines -> Z80ASM
runAfter_ assets Routines{..} = do
    ld HL $ getConst . afterTurn $ assets
    ld IX $ getConst . msgs1 $ assets
    jp runRatScript

parseLine_ :: Game (Const Location) -> Platform -> Vars -> Routines -> Z80ASM
parseLine_ assets Platform{..} Vars{..} Routines{..} = mdo
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
        ld A space
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

    parseError <- labelled do
        beforeParseError

        ld IX $ getConst . msgs1 $ assets
        ld B 1
        call printMessage
        jp readLine

    pure ()
  where
    connective = 100 -- TODO

-- | Print all items at location `D`.
--   Clobbers `IY`, `A`, `B`
printItemsAtD_ :: Game (Const Location) -> Platform -> Vars -> Routines -> Z80ASM
printItemsAtD_ Game{..} Platform{..} Vars{..} Routines{..} = do
    ld IY $ gameVars + fromIntegral maxItem
    decLoopB (maxItem - minItem) $ skippable \next -> do
        dec IY
        ld A [IY]
        cp D
        jp NZ next
        ld IX $ getConst msgs2
        push BC
        ld A B
        add A $ minItem - 1
        ld B A
        call printMessage
        pop BC
    ret


-- | Are there any items at location `D`? If yes, set Z
--   Clobbers `IY`, `A`, `B`
anyItemsAtD_ :: Game (Const Location) -> Vars -> Routines -> Z80ASM
anyItemsAtD_ Game{..} Vars{..} Routines{..} = do
    ld IY $ gameVars + fromIntegral maxItem
    decLoopB (maxItem - minItem) $ skippable \notHere -> do
        dec IY
        ld A [IY]
        cp D
        ret Z
    ld A 0
    cp 1
    ret

-- | Pre: `E is the variable's index
--   Post: `IY` is the variable's address
--   Clobbers: `D`, `A`, `IY`
varIY_ :: Vars -> Routines -> Z80ASM
varIY_ Vars{..} Routines{..} = do
    ld IY gameVars
    ld D 0
    add IY DE
    ret

qsave_ :: Vars -> Z80ASM
qsave_ Vars{..} = do
    forM_ savedVars \savedVars -> do
        ld DE savedVars
        ld HL gameVars
        ld BC 256
        ldir
    ret

gameLoop :: Game (Const Location) -> Platform -> Vars -> Z80 Routines
gameLoop assetLocs platform@Platform{..} vars@Vars{..} = mdo
    let routines = Routines{..}

    call resetGameVars
    call qsave

    newGame <- label

    clearScreen
    ldVia A [moved] 1

    withLabel \loop -> do
        skippable \notMoved -> do
            ld A [moved]
            dec A
            jp NZ notMoved
            ld [moved] A

            when False do -- Clear screen for each new location?
                clearScreen
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

        call runAfter
        jp loop

    gameOver <- labelled do
        -- `A` contains the player's status:
        -- 255: Dead
        -- 254: Won the game
        skippable \notDead -> do
            when supportScore $ call printScore
            call resetGameVars
            ldVia A [gameVars + 1] 0x00

            cp 254
            jp Z notDead
            message1 12
            ldVia A [gameVars + 1] 0xff

        waitEnter
        jp newGame

    -- Move all items at location `D` to location `E`. `
    -- Post: C` is number of items moved.
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

    runEnter <- labelled $ runEnter_ assetLocs platform vars routines
    runAfter <- labelled $ runAfter_ assetLocs routines
    runInteractiveBuiltin <- labelled $ runInteractiveBuiltin_ assetLocs platform vars routines
    runInteractiveLocal <- labelled $ runInteractiveLocal_ assetLocs routines
    runInteractiveGlobal <- labelled $ runInteractiveGlobal_ assetLocs routines
    runInteractive <- labelled $ runInteractive_ assetLocs routines

    runRatScript <- labelled $ runRatScript_ platform vars routines
    resetGameVars <- labelled $ resetGameVars_ assetLocs vars routines
    qsave <- labelled $ qsave_ vars

    findByRoom <- labelled $ findByRoom_ vars routines
    findByWords <- labelled $ findByWords_ vars routines

    anyItemsAtD <- labelled $ anyItemsAtD_ assetLocs vars routines
    printItemsAtD <- labelled $ printItemsAtD_ assetLocs platform vars routines
    varIY <- labelled $ varIY_ vars routines

    printScore <- labelled $ printScore_ platform vars routines

    pure routines
  where
    message1 msg = do
        ld IX $ getConst . msgs1 $ assetLocs
        ld B msg
        call printMessage

    Game
      { minItem = minItem
      , maxItem = maxItem
      } = assetLocs
