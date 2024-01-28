module Z80.Machine.HomeLab.HTP where

import Z80
import Z80.Utils

import qualified Data.ByteString as BS
import Data.List (intersperse)

htp :: Int -> BS.ByteString -> [ASMBlock] -> BS.ByteString
htp leaderLength label blocks = mconcat $
    [leader] <> intersperse (BS.singleton 0x01 <> leader) records <> [BS.singleton 0x00]
  where
    leader = BS.replicate leaderLength 0x00
    records = zipWith record (label:repeat mempty) blocks

    record label block = mconcat
        [ BS.singleton 0xa5
        , label
        , BS.singleton 0x00
        , word $ asmOrg block
        , word . fromIntegral $ BS.length bs
        , bs
        , crc bs
        ]
      where
        bs = asmData block

    crc = BS.singleton . BS.foldr' (+) 0

    word w = BS.pack [lo, hi]
      where
        (lo, hi) = wordBytes w
