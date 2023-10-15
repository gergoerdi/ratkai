{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
module RatBC.Engine where

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
import Data.Char
import Control.Arrow (first)
import Text.Printf
import System.Console.Haskeline
import Data.List.Split
import Debug.Trace
import GHC.Stack

type Ptr = Int

data St = MkSt
  { vars :: IOArray Word8 Word8
  }

newtype Engine m a = Engine{ unEngine :: RWST (IOArray Word8 Word8) Any () m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadMask, MonadWriter Any)

runEngine :: (Monad m) => IOArray Word8 Word8 -> Engine m a -> m (a, Any)
runEngine vars engine = evalRWST (unEngine engine) vars ()

dumpVars :: (MonadIO m) => Engine m ()
dumpVars = do
    vars <- Engine ask
    liftIO do
        vars <- freeze vars :: IO (Array Word8 Word8)
        forM_ (chunksOf 16 $ elems vars) \chunk -> do
            mapM_ (printf "%02x ") chunk
            printf "\n"

newtype Terp m a = Terp{ unTerp :: RWST (ByteString, ByteString) () Ptr (Engine m) a }
    deriving (Functor, Applicative, Monad, MonadIO)

runTerp :: (MonadIO m) => ByteString -> ByteString -> Engine m ()
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
    vars <- Engine ask
    liftIO $ readArray vars var

getVar :: (MonadIO m) => Word8 -> Terp m Word8
getVar = Terp . lift . getVar'

putVar' :: (MonadIO m) => Word8 -> Word8 -> Engine m ()
putVar' var val = do
    vars <- Engine ask
    liftIO $ writeArray vars var val

putVar :: (MonadIO m) => Word8 -> Word8 -> Terp m ()
putVar var = Terp . lift . putVar' var

unpackZ :: (HasCallStack) => ByteString -> ([Word8], ByteString)
unpackZ zs = first ([b1, b2, b3]<>) (if final then ([], zs2) else unpackZ zs2)
  where
    Just (lo, zs1) = BS.uncons zs
    Just (hi, zs2) = BS.uncons zs1

    b3 = lo .&. 0x1f
    b2 = (lo `shiftR` 5 .|. hi `shiftL` 3) .&. 0x1f
    b1 = (hi `shiftR` 2) .&. 0x1f
    final = hi `testBit` 7

decodeZ1 :: Word8 -> RWS () [Char] Bool ()
decodeZ1 b = do
    shifted <- get
    put False
    if shifted then shift b else regular b
  where
    regular 0 = pure ()
    regular 1 = put True
    regular 2 = tell " "
    regular 3 = tell "."
    regular 4 = tell ","
    regular 5 = tell "\n"
    regular b = tell [chr $ 0x41 - 6 + fromIntegral b]

    shift 0 = pure ()
    shift 1 = tell "?"
    shift 2 = tell "'"
    shift 3 = tell ":"
    shift 4 = tell "-"
    shift 5 = tell "&"
    shift 6 = tell "!"
    shift 7 = tell "<CLR>"
    shift 8 = tell "\t"
    shift b | b >= 22 = tell [chr $ 0x30 - 22 + fromIntegral b]

decodeZ :: [Word8] -> String
decodeZ bs = s
  where
    (_, s) = execRWS (mapM_ decodeZ1 bs) () False

findZ :: ByteString -> Word8 -> ByteString
findZ bs 1 = bs
findZ bs i = let (_, bs') = unpackZ bs in findZ bs' (i - 1)

printZ :: (MonadIO m) => ByteString -> Word8 -> m ()
printZ bank = liftIO . mapM_ putStrLn . reflow . decodeZ . fst . unpackZ . findZ bank
  where
    reflow :: String -> [String]
    reflow = concatMap (chunksOf 40) . split (dropDelims . onSublist $ "\n")

printlnZ :: (MonadIO m) => ByteString -> Word8 -> m ()
printlnZ bank msg = printZ bank msg >> liftIO (putStrLn "")

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

runBC :: (MonadIO m) => Terp m ()
runBC = do
    op <- fetch
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
        0x08 -> assertVar =<< getVar playerLoc
        0x09 -> skip >> runBC
        0x0a -> skipUnless 0x00
        0x0b -> skipUnless 0xff
        0x0c -> fetch >>= putVar playerLoc >> moved
        0x0d -> fetch >>= putVar playerStatus >> runBC
        0x0e -> inc playerHealth >> runBC
        0x0f -> dec playerHealth >> runBC
        0x10 -> inc playerScore >> runBC
        0x15 -> fetch >> liftIO (putStrLn "<SLEEP>") >> runBC
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
        -- if val /= target then message msg >> skip else seek 1
        -- runBC

    skipUnless target = do
        var <- fetch
        val <- getVar var
        if val /= target then skip else seek 1
        runBC

    message msg = do
        bank <- Terp $ asks fst
        printlnZ bank msg

    skip = peek >>= seek

    inc var = do
        delta <- fetch
        val <- getVar var
        putVar var $ min 99 (val + delta)

    dec var = do
        delta <- fetch
        val <- getVar var
        putVar var $ max 0 (val - delta)

runBuiltin :: (MonadIO m) => [Word8] -> Engine m ()
runBuiltin input = pure ()
