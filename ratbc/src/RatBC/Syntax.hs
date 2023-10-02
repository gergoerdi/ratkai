{-# LANGUAGE LambdaCase #-}
module RatBC.Syntax where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative
import Text.Printf
import Debug.Trace
import qualified Data.ByteString.Lazy as BL

type Addr = Word16
type Var = Word8
type Val = Word8
type Msg = Val

data Stmt
    = Ret
    | Assign Var Val
    | Message Msg
    | Assign00 Var
    | AssignFF Var
    | AssignLoc Var
    | Assert00 Var Msg
    | AssertFF Var Msg
    | AssertHere Var Msg
    | Skip Val
    | If00 Var [Stmt]
    | IfFF Var [Stmt]
    | MoveTo Val
    | SetPlayerStatus Val
    | Heal Val
    | Hurt Val
    | AddScore Val
    | SetScreen Val Val Val
    | SpriteOn Val Val Val Val Val
    | SpriteOff Val
    | Chime Val
    | Sleep Val
    | IncIfNot0 Var
    | MachineCode Addr [Val]
    | CopyProtection Val Val Val Val
    deriving (Show, Read)

instance Binary Stmt where
    get = getWord8 >>= {- (\x -> pure $ traceShowId x) >>= -} \case
        0x00 -> pure Ret
        0x01 -> Assign <$> get <*> get
        0x02 -> Message <$> get
        0x03 -> Assign00 <$> get
        0x04 -> AssignFF <$> get
        0x05 -> AssignLoc <$> get
        0x06 -> Assert00 <$> get <*> get
        0x07 -> AssertFF <$> get <*> get
        0x09 -> Skip <$> get
        0x0a -> If00 <$> get <*> getBlock
        0x0b -> IfFF <$> get <*> getBlock
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
        tag -> error $ printf "get: invalid instruction 0x%02x" tag

    put = \case
        Ret -> putWord8 0x00
        Assign var val -> putWord8 0x01 *> put var *> put val
        Message msg -> putWord8 0x02 *> put msg
        Assign00 var -> putWord8 0x03 *> put var
        AssignFF var -> putWord8 0x04 *> put var
        AssignLoc var -> putWord8 0x05 *> put var
        Assert00 var msg -> putWord8 0x06 *> put var *> put msg
        AssertFF var msg -> putWord8 0x07 *> put var *> put msg
        Skip len -> putWord8 0x09 *> put len
        If00 var body -> putWord8 0x0a *> put var *> putBlock body
        IfFF var body -> putWord8 0x0b *> put var *> putBlock body
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
        stmt -> error $ printf "put: %s" (show stmt)

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
            if here >= end then pure [] else get <:> loop
    loop

putBlock :: [Stmt] -> Put
putBlock stmts = do
    let bs = runPut $ mapM_ put stmts
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

whileValid :: Get [Stmt]
whileValid = do
    tag <- lookAhead getWord8
    if tag > 0x18 then pure [] else do
        x <- get
        -- traceShowM x
        pure x <:> whileValid

getStmts :: Get [Stmt]
getStmts = untilEOF get
