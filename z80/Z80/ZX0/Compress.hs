{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BlockArguments #-}
module Z80.ZX0.Compress where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import qualified Data.ByteString as BS
import Data.Word

data Block

foreign import ccall "optimize" c_optimize :: Ptr CChar -> CInt -> CInt -> CInt -> IO (Ptr Block)
foreign import ccall "compress" c_compress :: Ptr Block -> Ptr CChar -> CInt -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO (Ptr CChar)

maxOffset :: CInt
maxOffset = 32640

compressBackwards :: BS.ByteString -> IO (BS.ByteString, Word16)
compressBackwards bs = do
    bs <- pure $ BS.reverse bs
    (bs', delta) <- alloca \outSize -> alloca \delta -> do
        ptr' <- BS.useAsCStringLen bs \(ptr, len) -> do
            block <- c_optimize ptr (fromIntegral len) 0 maxOffset
            c_compress block ptr (fromIntegral len) 0 1 0 outSize delta
        outSize <- peek outSize
        delta <- peek delta
        bs' <- BS.packCStringLen (ptr', fromIntegral outSize)
        pure (bs', fromIntegral delta)
    bs' <- pure $ BS.reverse bs'
    pure (bs', delta)
