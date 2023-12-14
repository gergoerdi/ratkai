{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
module RatBC.Engine.ZSCII where

import Control.Monad.RWS
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import Data.Char
import Control.Arrow (first)
import Data.List.Split
import GHC.Stack

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
