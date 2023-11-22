{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import qualified Ratkai.TVC as Ratkai
import Z80.TVC.Format (cas)

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
    let inputPath = "game/bosszu/full"

    assets <- loadTextFiles inputPath
    emit "_build/tvc/ratkai" $ Ratkai.game assets

emit :: String -> Z80ASM -> IO ()
emit name prog = do
    createDirectoryIfMissing True (takeDirectory name)
    let bin = asmData block
    BS.writeFile (name <.> "obj") bin
    printf "Raw binary size: %d bytes\n" $ BS.length bin
    BS.writeFile (name <.> "cas") $ cas block
  where
    block = org 0x1a00 prog
