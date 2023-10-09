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

data InputDispatch = InputDispatch [Word8] [Stmt]
    deriving (Show, Read)

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
    -- | If00 Var [Stmt]
    -- | IfFF Var [Stmt]
    | If00 Var Val
    | IfFF Var Val
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
    | MachineCode Addr [Word8]
    | CopyProtection Val Val Val Val
    deriving (Show, Read)

instance Binary Stmt where
    get = getStmt >>= \case
        Left byte -> error $ printf "get: invalid instruction 0x%02x" byte
        Right stmt -> pure stmt

    put = \case
        Ret -> putWord8 0x00
        Assign var val -> putWord8 0x01 *> put var *> put val
        Message msg | msg <= 0x18 -> putWord8 0x02 *> put msg
                    | otherwise -> putWord8 msg
        Assign00 var -> putWord8 0x03 *> put var
        AssignFF var -> putWord8 0x04 *> put var
        AssignLoc var -> putWord8 0x05 *> put var
        Assert00 var msg -> putWord8 0x06 *> put var *> put msg
        AssertFF var msg -> putWord8 0x07 *> put var *> put msg
        AssertHere var msg -> putWord8 0x08 *> put var *> put msg
        Skip len -> putWord8 0x09 *> put len
        -- If00 var body -> putWord8 0x0a *> put var *> putBlock body
        -- IfFF var body -> putWord8 0x0b *> put var *> putBlock body
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
        MachineCode addr bytes -> putWord8 0x17 *> mapM_ putWord8 bytes
        CopyProtection a1 a2 a3 a4 -> putWord8 0x18 *> put a1 *> put a2 *> put a3 *> put a4

getMachineCode :: Get [Word8]
getMachineCode = do
    b <- getWord8
    if b == 0x60 then pure [b] else pure b <:> getMachineCode

getStmt :: Get (Either Word8 Stmt)
getStmt = getWord8 >>= {- (\x -> pure $ traceShowId x) >>= -} \case
    0x00 -> fmap Right $ pure Ret
    0x01 -> fmap Right $ Assign <$> get <*> get
    0x02 -> fmap Right $ Message <$> get
    0x03 -> fmap Right $ Assign00 <$> get
    0x04 -> fmap Right $ AssignFF <$> get
    0x05 -> fmap Right $ AssignLoc <$> get
    0x06 -> fmap Right $ Assert00 <$> get <*> get
    0x07 -> fmap Right $ AssertFF <$> get <*> get
    0x08 -> fmap Right $ AssertHere <$> get <*> get
    0x09 -> fmap Right $ Skip <$> get
    -- 0x0a -> fmap Right $ If00 <$> get <*> getBlock
    -- 0x0b -> fmap Right $ IfFF <$> get <*> getBlock
    0x0a -> fmap Right $ If00 <$> get <*> get
    0x0b -> fmap Right $ IfFF <$> get <*> get
    0x0c -> fmap Right $ MoveTo <$> get
    0x0d -> fmap Right $ SetPlayerStatus <$> get
    0x0e -> fmap Right $ Heal <$> get
    0x0f -> fmap Right $ Hurt <$> get
    0x10 -> fmap Right $ AddScore <$> get
    0x11 -> fmap Right $ SetScreen <$> get <*> get <*> get
    0x12 -> fmap Right $ SpriteOn <$> get <*> get <*> get <*> get <*> get
    0x13 -> fmap Right $ SpriteOff <$> get
    0x14 -> fmap Right $ Chime <$> get
    0x15 -> fmap Right $ Sleep <$> get
    0x16 -> fmap Right $ IncIfNot0 <$> get
    0x17 -> fmap Right $ MachineCode <$> (fromIntegral <$> bytesRead) <*> getMachineCode
    0x18 -> fmap Right $ CopyProtection <$> get <*> get <*> get <*> get
    tag -> pure $ Left tag


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
