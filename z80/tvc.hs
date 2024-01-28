{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import qualified Target.TVC as Ratkai
import Z80.Machine.TVC.Cas (cas)

import RatBC.Game.Text
import RatBC.Game
import RatBC.TVC.Binary
import RatBC.TVC.Text
import RatBC.TVC.Strip

import Z80
import Z80.Utils
import Z80.ZX0.Compress
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Bifunctor
import Data.Functor.Const
import Data.String (fromString)
import System.FilePath
import System.Directory
import Text.Printf

main :: IO ()
main = do
    let inputPath = "game/bosszu/full"

    assets <- loadTextFiles inputPath
    let assets' = mapGameF (first BL.toStrict) . assemble . reflowMessages 31 . preprocessGame $ assets

    let zx0 bs = do
            (bs', delta) <- compressBackwards bs
            pure (bs, bs', delta)
    text1 <- zx0 $ getConst . msgs1 $ assets'
    text2 <- zx0 $ getConst . msgs2 $ assets'
    pics <- zx0 =<< BS.readFile (inputPath </> "pics-tvc.bin")
    emit "_build/tvc/ratkai" $ Ratkai.game assets' text1 text2 pics

emit :: String -> Z80ASM -> IO ()
emit name prog = do
    createDirectoryIfMissing True (takeDirectory name)
    let bin = asmData block
    BS.writeFile (name <.> "obj") bin
    printf "Raw binary size: %d bytes\n" $ BS.length bin
    BS.writeFile (name <.> "cas") $ cas block
  where
    block = org 0x1a00 prog
