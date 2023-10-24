{-# LANGUAGE LambdaCase, RecordWildCards, TypeApplications #-}
module RatBC.Commodore64.Binary where

import RatBC.Utils
import RatBC.Syntax
import RatBC.Words
import RatBC.Text
import RatBC.Game

import Control.Monad.Identity
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Data.List.Split
import Text.Printf
import Data.Array (listArray)
import Data.List.Split
import Data.Either

import Debug.Trace

fromImage :: BL.ByteString -> Game Identity
fromImage bs = Game
    { msgs1 = Identity $ listArray (1, fromIntegral $ length strings1) strings1
    , msgs2 = Identity $ listArray (1, fromIntegral $ length strings2) strings2
    , dict = Identity $ loadWords bs
    , enterRoom = Identity $
        let bs' = BL.tail $ deref bs 0x0827
            bss = map BL.pack . take numRooms . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs'
        in listArray (1, fromIntegral numRooms) $ map getStmts bss
    , afterTurn = Identity $
        let bs' = deref bs 0x082b
            bss = map BL.pack . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs'
        in getStmts (head bss)
    , interactiveGlobal = Identity $
        let bs' = deref bs 0x082d
        in interactive bs'
    , interactiveLocal = Identity $
        let bs' = BL.tail $ deref bs 0x0829
            bss = take numRooms $ map BL.pack . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs'
        in listArray (1, fromIntegral numRooms) $ map interactive bss
    , resetState = Identity $
        let bs' = deref bs 0x0835
        in BL.take (fromIntegral $ maxItem - minItem) bs'
    , helpMap = Identity $
        let bs' = deref bs 0x0837
        in listArray (1, fromIntegral numRooms) $ BL.unpack bs'
    , minItem = minItem
    , maxItem = maxItem
    , startRoom = BL.index bs 0x083c
    }
  where
    numRooms = 97

    minItem = BL.index bs 0x083a
    maxItem = BL.index bs 0x083b

    (strings1, strings2) = loadMessages bs

    getStmts bs = runGet loop bs
      where
        loop = do
            empty <- isEmpty
            if empty then pure [] else do
                tag <- lookAhead getWord8
                if tag > 0x18 then pure [] else getStmt <:> loop

    interactive bs =
        let bss = map BL.pack . takeWhile (not . null) . splitOn [0x00, 0x00] . BL.unpack $ bs
        in [ InputDispatch (BL.unpack input) (getStmts action)
           | bs <- bss
           , let (input, action) = BL.break (== 0x00) bs
           , not $ BL.null action
           , action <- pure $ BL.tail action
           ]

getStmt :: Get Stmt
getStmt = getWord8 >>= {- (\x -> pure $ traceShowId x) >>= -} \case
    0x00 -> pure Ret
    0x01 -> Assign <$> get <*> get
    0x02 -> Message <$> get
    0x03 -> Assign00 <$> get
    0x04 -> AssignFF <$> get
    0x05 -> AssignLoc <$> get
    0x06 -> Assert00 <$> get <*> get
    0x07 -> AssertFF <$> get <*> get
    0x08 -> AssertHere <$> get <*> get
    0x09 -> Skip <$> get
    0x0a -> If00 <$> get <*> get
    0x0b -> IfFF <$> get <*> get
    0x0c -> MoveTo <$> get
    0x0d -> SetPlayerStatus <$> get
    0x0e -> Heal <$> get
    0x0f -> Hurt <$> get
    0x10 -> AddScore <$> get
    0x11 -> SetScreen <$> get <*> get <*> get
    0x12 -> SpriteOn <$> get <*> get <*> get <*> get <*> get
    0x13 -> SpriteOff <$> get
    0x14 -> Chime <$> get
    0x15 -> Sleep <$> get
    0x16 -> IncIfNot0 <$> get
    0x17 -> MachineCode <$> (fromIntegral <$> bytesRead) <*> getMachineCode
    0x18 -> CopyProtection <$> get <*> get <*> get <*> get
    tag -> error $ printf "get: invalid instruction 0x%02x" tag

putStmt :: Stmt -> Put
putStmt = \case
    Ret -> putWord8 0x00
    Assign var val -> putWord8 0x01 *> put var *> put val
    CompactMessage msg -> putWord8 0x02 *> put msg
    Message msg -> putWord8 0x02 *> put msg
    Assign00 var -> putWord8 0x03 *> put var
    AssignFF var -> putWord8 0x04 *> put var
    AssignLoc var -> putWord8 0x05 *> put var
    Assert00 var msg -> putWord8 0x06 *> put var *> put msg
    AssertFF var msg -> putWord8 0x07 *> put var *> put msg
    AssertHere var msg -> putWord8 0x08 *> put var *> put msg
    Skip len -> putWord8 0x09 *> put len
    When00 var body -> putWord8 0x0a *> put var *> putBlock body
    WhenFF var body -> putWord8 0x0b *> put var *> putBlock body
    If00 var len -> putWord8 0x0a *> put var *> put len
    IfFF var len -> putWord8 0x0b *> put var *> put len
    MoveTo loc -> putWord8 0x0c *> put loc
    SetPlayerStatus val -> putWord8 0x0d *> put val
    Heal val -> putWord8 0x0e *> put val
    Hurt val -> putWord8 0x0f *> put val
    AddScore val -> putWord8 0x10 *> put val
    SetScreen a1 a2 a3 -> putWord8 0x11 *> put a1 *> put a2 *> put a3
    SpriteOn a1 a2 a3 a4 a5 -> putWord8 0x12 *> put a1 *> put a2 *> put a3 *> put a4 *> put a5
    SpriteOff a1 -> putWord8 0x13 *> put a1
    Chime val -> putWord8 0x14 *> put val
    Sleep val -> putWord8 0x15 *> put val
    IncIfNot0 var -> putWord8 0x16 *> put var
    MachineCode _addr bytes -> putWord8 0x17 *> mapM_ putWord8 bytes
    CopyProtection a1 a2 a3 a4 -> putWord8 0x18 *> put a1 *> put a2 *> put a3 *> put a4

getMachineCode :: Get [Word8]
getMachineCode = do
    b <- getWord8
    if b == 0x60 then pure [b] else pure b <:> getMachineCode

getBlock :: Get [Stmt]
getBlock = do
    len <- getWord8
    -- body <- getLazyByteString (fromIntegral len - 1)
    -- traceShowM (len, BL.unpack body)
    -- pure $ runGet (untilEOF get) body
    start <- bytesRead
    let end = start + fromIntegral (len - 1)
    let loop = do
            here <- bytesRead
            if here >= end then pure [] else getStmt <:> loop
    loop

putBlock :: [Stmt] -> Put
putBlock stmts = do
    let bs = runPut $ mapM_ putStmt stmts
        len = BL.length bs
    putWord8 $ fromIntegral len + 1
    putLazyByteString bs

infixr 5 <:>
(<:>) :: (Applicative f) => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

untilEOF :: Get a -> Get [a]
untilEOF get = do
    empty <- isEmpty
    if empty then pure [] else get <:> untilEOF get

getStmts :: Get [Stmt]
getStmts = untilEOF getStmt

restoreBlocks :: [Stmt] -> [Stmt]
restoreBlocks [] = []
restoreBlocks (stmt:stmts) = case stmt of
    If00 var len -> restoreBlock (When00 var) len stmts
    IfFF var len -> restoreBlock (WhenFF var) len stmts
    _ -> stmt : restoreBlocks stmts
  where
    restoreBlock :: ([Stmt] -> Stmt) -> Word8 -> [Stmt] -> [Stmt]
    restoreBlock mkBlock len stmts =
        let (stmt', stmts') = go [] (len - 1) stmts
        in stmt' : restoreBlocks stmts'
      where
        go :: [Stmt] -> Word8 -> [Stmt] -> (Stmt, [Stmt])
        go acc 0 stmts = success acc stmts
        go acc len [] = failure
        go acc len (stmt:stmts)
          | this <= len = go (stmt:acc) (len - this) stmts
          | otherwise = failure
          where
            this = fromIntegral . BL.length $ runPut . putStmt $ stmt

        failure :: (Stmt, [Stmt])
        failure = (stmt, stmts)

        success :: [Stmt] -> [Stmt] -> (Stmt, [Stmt])
        success acc rest = (mkBlock (restoreBlocks $ reverse acc), rest)
