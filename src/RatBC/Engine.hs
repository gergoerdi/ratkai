{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
module RatBC.Engine where

import RatBC.Game (Bank(..))
import RatBC.Engine.ZSCII

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.Catch
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Data.Array.IO
import Data.Array (Array, elems)
import Data.Bits
import Control.Arrow (first)
import Text.Printf
import Data.List.Split
import Debug.Trace
import GHC.Stack

type Ptr = Int

data R = MkR
  { vars :: IOArray Word8 Word8
  , helpMap :: ByteString
  }

newtype Engine m a = Engine{ unEngine :: RWST R Any () m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadMask, MonadWriter Any)

class (Monad m) => MonadMessage m where
    printMessage :: Bank -> Word8 -> m ()

instance MonadMessage m => MonadMessage (Engine m) where
    printMessage bank msg = Engine . lift $ printMessage bank msg

runEngine :: (Monad m) => IOArray Word8 Word8 -> ByteString -> Engine m a -> m (a, Any)
runEngine vars helpMap engine = evalRWST (unEngine engine) (MkR vars helpMap) ()

dumpVars :: (MonadIO m) => Engine m ()
dumpVars = do
    vars <- Engine $ asks vars
    liftIO do
        vars <- freeze vars :: IO (Array Word8 Word8)
        forM_ (zip [0, 16..] $ chunksOf 16 $ elems vars) \(i, chunk) -> do
            printf "%02x  " (i :: Int)
            mapM_ (printf "%02x ") chunk
            printf "\n"

newtype Terp m a = Terp{ unTerp :: RWST (Bank, ByteString) () Ptr (Engine m) a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadMessage m => MonadMessage (Terp m) where
    printMessage bank msg = Terp . lift $ printMessage bank msg

runTerp :: (MonadIO m, MonadMessage m) => Bank -> ByteString -> Engine m ()
runTerp bank bc = do
    runRWST (unTerp runBC) (bank, bc) 0
    pure ()

moved :: (Monad m) => Terp m ()
moved = Terp . lift . tell $ Any True

peek :: (Monad m) => Terp m Word8
peek = do
    ptr <- Terp get
    bs <- Terp $ asks snd
    pure $ BS.index bs ptr

seek :: (Monad m) => Word8 -> Terp m ()
seek n = Terp $ modify (+ fromIntegral n)

fetch :: (Monad m) => Terp m Word8
fetch = peek <* seek 1

playerLoc, playerStatus, playerHealth, playerScore :: Word8
playerScore = 0xfc
playerHealth = 0xfd
playerStatus = 0xfe
playerLoc = 0xff

getVar' :: (MonadIO m) => Word8 -> Engine m Word8
getVar' var = do
    vars <- Engine $ asks vars
    liftIO $ readArray vars var

getVar :: (MonadIO m) => Word8 -> Terp m Word8
getVar = Terp . lift . getVar'

putVar' :: (MonadIO m) => Word8 -> Word8 -> Engine m ()
putVar' var val = do
    vars <- Engine $ asks vars
    liftIO $ writeArray vars var val

putVar :: (MonadIO m) => Word8 -> Word8 -> Terp m ()
putVar var = Terp . lift . putVar' var

unpackWords :: ByteString -> [(String, Word8)]
unpackWords bs
    | b0 == 0xff = []
    | otherwise = (w, b) : unpackWords bs''
  where
    b0 = BS.head bs

    (z, bs') = unpackZ bs
    w = decodeZ z
    Just (b, bs'') = BS.uncons bs'

leap :: ByteString -> Word8 -> ByteString
leap bs 1 = BS.tail bs
leap bs i = let Just (b, bs') = BS.uncons bs
            in leap (BS.drop (fromIntegral b) bs) (i - 1)

findByRoom :: (MonadIO m) => ByteString -> Engine m ByteString
findByRoom bs = do
    loc <- getVar' playerLoc
    pure $ leap bs loc

findByInput :: [Word8] -> ByteString -> Maybe ByteString
findByInput input bs = do
    guard $ len /= 0
    -- traceShowM $ BS.unpack $ BS.take 10 bs
    match input bs' `mplus` findByInput input (BS.drop (fromIntegral len) bs)
  where
    Just (len, bs') = BS.uncons bs

    match ws bs
        | [] <- ws, b0 == 0x00 = Just bs'
        | (w:ws) <- ws, w == b0 = match ws bs'
        | otherwise = Nothing
      where
        Just (b0, bs') = BS.uncons bs

runBC :: (MonadIO m, MonadMessage m) => Terp m ()
runBC = do
    op <- fetch
    -- liftIO $ printf "runBC: 0x%02x\n" op
    if op > 0x18 then message op >> runBC else case op of
        0x00 -> pure ()
        0x01 -> do
            var <- fetch
            val <- fetch
            putVar var val
            runBC
        0x02 -> fetch >>= message >> runBC
        0x03 -> do
            var <- fetch
            putVar var 0x00
            runBC
        0x04 -> do
            var <- fetch
            putVar var 0xff
            runBC
        0x05 -> do
            var <- fetch
            putVar var =<< getVar playerLoc
            runBC
        0x06 -> assertVar 0x00
        0x07 -> assertVar 0xff
        0x08 -> assertHere
        0x09 -> skip >> runBC
        0x0a -> skipUnless 0x00
        0x0b -> skipUnless 0xff
        0x0c -> fetch >>= putVar playerLoc >> moved
        0x0d -> do
            status <- fetch
            liftIO $ printf "playerStatus = %d\n" status
            putVar playerStatus status
            runBC
        0x0e -> inc playerHealth >> runBC
        0x0f -> dec playerHealth >> runBC
        0x10 -> inc playerScore >> runBC
        0x15 -> fetch >>= \n -> liftIO (printf "<SLEEP %d>\n" n) >> runBC
        0x16 -> do
            var <- fetch
            val <- getVar var
            unless (val == 0) $ putVar var (val + 1)
        _ -> error $ printf "0x%02x" op
  where
    assertVar target = do
        var <- fetch
        msg <- fetch
        val <- getVar var
        if val /= target then message msg else runBC

    assertHere = do
        var <- fetch
        msg <- fetch
        val <- getVar var
        loc <- getVar playerLoc
        if val `notElem` [loc, 0x00] then message msg else runBC

    skipUnless target = do
        var <- fetch
        val <- getVar var
        if val /= target then skip else seek 1
        runBC

    message msg = do
        bank <- Terp $ asks fst
        printMessage bank msg

    skip = peek >>= seek

    inc var = do
        delta <- fetch
        val <- getVar var
        putVar var $ min 99 (val + delta)

    dec var = do
        delta <- fetch
        val <- getVar var
        putVar var $ max 0 (val - delta)

runBuiltin :: (MonadIO m, MonadMessage m) => [Word8] -> Engine m ()
runBuiltin = \case
    [0x0e] -> do -- Take all
        loc <- getVar' playerLoc
        items <- filterM (\i -> (loc ==) <$> getVar' i) [minItem..maxItem]
        if null items then printMessage Bank1 9 else do
            forM_ items \i -> putVar' i 0x00
            printMessage Bank1 4
    [0x0e, i] -> do -- Take
        loc <- getVar' playerLoc
        when (minItem <= i && i <= maxItem) do
            val <- getVar' i
            if val /= loc then printMessage Bank1 5 else do
                putVar' i 0x00
    [0x0f] -> do -- Drop all
        loc <- getVar' playerLoc
        items <- filterM (\i -> (0 ==) <$> getVar' i) [minItem..maxItem]
        if null items then printMessage Bank1 8 else do
            forM_ items \i -> putVar' i loc
            printMessage Bank1 4
    [0x0f, i] -> do -- Drop
        loc <- getVar' playerLoc
        when (minItem <= i && i <= maxItem) do
            val <- getVar' i
            if val /= 0 then printMessage Bank1 6 else do
                putVar' i loc
    [0x10] -> do -- Inventory
        printMessage Bank1 7
        items <- filterM (\i -> (0 ==) <$> getVar' i) [minItem..maxItem]
        if null items then printMessage Bank1 8 else do
            mapM_ (printMessage Bank2) items
    [0x16] -> do -- Help
        loc <- getVar' playerLoc
        helps <- Engine $ asks helpMap
        case BS.index helps (fromIntegral loc - 1) of
            0x00 -> printMessage Bank1 10
            msg -> printMessage Bank2 msg
    (w:_) -> do
        printMessage Bank1 $ case w of
            w | w <= 12 -> 3
            _ -> 2

minItem, maxItem :: Word8
minItem = 120
maxItem = 160 - 1
