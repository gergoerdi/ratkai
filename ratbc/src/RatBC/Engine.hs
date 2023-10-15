{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
module RatBC.Engine where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.RWS
import Data.ByteString as BS
import Data.Word
import Data.Array.IO
import Data.Bits
import Data.Char
import Control.Arrow (first)

type Ptr = Int

data St = MkSt
  { vars :: IOArray Word8 Word8
  }

newtype Engine m a = Engine{ unEngine :: ReaderT (IOArray Word8 Word8) m a }
    deriving (Functor, Applicative, Monad, MonadIO)

newtype Terp m a = Terp{ unTerp :: RWST (ByteString, ByteString) () Ptr (Engine m) a }
    deriving (Functor, Applicative, Monad)

peek :: (Monad m) => Terp m Word8
peek = do
    ptr <- Terp get
    (_, bs) <- Terp ask
    pure $ BS.index bs ptr

fetch :: (Monad m) => Terp m Word8
fetch = peek <* Terp (modify (+ 1))

getVar :: (MonadIO m) => Word8 -> Engine m Word8
getVar var = undefined

putVar :: (MonadIO m) => Word8 -> Word8 -> Engine m ()
putVar var val = undefined

printZ :: (MonadIO m) => ByteString -> Word8 -> Engine m ()
printZ bs idx = undefined

unpackZ :: ByteString -> ([Word8], ByteString)
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
    shift b | b >= 22 = tell [chr $ 0x30 - 22 + 1 + fromIntegral b]

decodeZ :: [Word8] -> String
decodeZ bs = s
  where
    (_, s) = execRWS (mapM_ decodeZ1 bs) () False

findZ :: ByteString -> Word8 -> ByteString
findZ bs 1 = bs
findZ bs i = let (_, bs') = unpackZ bs in findZ bs' (i - 1)

unpackWords :: ByteString -> [(String, Word8)]
unpackWords bs
    | b0 == 0xff = []
    | otherwise = (w, b) : unpackWords bs''
  where
    b0 = BS.head bs

    (z, bs') = unpackZ bs
    w = decodeZ z
    Just (b, bs'') = BS.uncons bs'
