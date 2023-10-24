module RatBC.Utils where

import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as BL


deref :: BL.ByteString -> Word16 -> BL.ByteString
deref bs ptr = BL.drop addr bs
  where
    lo = BL.index bs (fromIntegral ptr)
    hi = BL.index bs (fromIntegral ptr + 1)
    addr = fromIntegral hi `shiftL` 8 .|. fromIntegral lo
