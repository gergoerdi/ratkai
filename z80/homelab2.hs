{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import qualified Target.HomeLab2 as Ratkai
import RatBC.Game.Text

import Z80
import Z80.Utils
import qualified Data.ByteString as BS
import Data.String (fromString)
import System.FilePath
import System.Directory
import Text.Printf

main :: IO ()
main = do
    let inputPath = "game/bosszu/part1"
    assets <- loadTextFiles inputPath
    -- assets <- pure $ mapStmts (\_ -> restoreBlocks) assets
    emit "_build/homelab2/ratkai" . org (16700 - 60) $ Ratkai.game assets

emit :: String -> ASMBlock -> IO ()
emit name block = do
    createDirectoryIfMissing True (takeDirectory name)
    let bin = asmData block
    BS.writeFile (name <.> "obj") bin
    printf "Raw binary size: %d bytes\n" $ BS.length bin
    BS.writeFile (name <.> "htp") $ htp (fromString $ takeBaseName name) block

htp :: BS.ByteString -> ASMBlock -> BS.ByteString
htp label mainBlock = mconcat
    [ leader
    , record label $ org 0x4002 do
            dw [asmOrg mainBlock]
    , BS.singleton 0x01
    , leader
    , record mempty mainBlock
    , BS.singleton 0x00
    ]
  where
    leader = BS.replicate 100 0x00

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
