{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import qualified Target.HomeLab34 as Ratkai
import RatBC.Game.Text

import Z80
import Z80.Utils
import Z80.Machine.HomeLab.HTP
import qualified Data.ByteString as BS
import Data.String (fromString)
import System.FilePath
import System.Directory
import Text.Printf

main :: IO ()
main = do
    let inputPath = "game/bosszu/full.homelab34"
    assets <- loadTextFiles inputPath
    -- assets <- pure $ mapStmts (\_ -> restoreBlocks) assets
    emit "_build/homelab34/ratkai" . org 16700 $ Ratkai.game assets

emit :: String -> ASMBlock -> IO ()
emit name block = do
    createDirectoryIfMissing True (takeDirectory name)
    let bin = asmData block
    BS.writeFile (name <.> "obj") bin
    printf "Raw binary size: %d bytes\n" $ BS.length bin
    BS.writeFile (name <.> "htp") $ htpWithAutoStart (fromString $ takeBaseName name) block

htpWithAutoStart :: BS.ByteString -> ASMBlock -> BS.ByteString
htpWithAutoStart label mainBlock = htp 200 label
    [ mainBlock
    , org 0x4002 $ dw [asmOrg mainBlock, asmOrg mainBlock]
    ]
