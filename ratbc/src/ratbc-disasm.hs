{-# LANGUAGE ApplicativeDo, RecordWildCards, TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RatBC.Utils
import RatBC.Syntax
import RatBC.Words
import RatBC.Text
import RatBC.Game
import RatBC.Pretty

import Control.Monad.Identity
import Data.Array (listArray)
import Prettyprinter
import Prettyprinter.Render.String
import Data.String
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Options.Applicative
import Control.Monad
import Data.List.Split
import Text.Printf
import Data.Word
import Data.Maybe
import Data.Either
import Data.Char
import System.Directory
import System.FilePath

data Options = Options
    { inputPath :: FilePath
    , outputPath :: FilePath
    }

fromImage :: BL.ByteString -> Game Identity
fromImage bs = Game
    { msgs1 = Identity $ listArray (1, fromIntegral $ length strings1) strings1
    , msgs2 = Identity $ listArray (1, fromIntegral $ length strings2) strings2
    , dict = Identity $ loadWords bs
    , enterRoom = Identity $
        let bs' = BL.tail $ deref bs 0x0827
            bss = map BL.pack . take numRooms . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs'
        in listArray (1, fromIntegral numRooms) $ map getStmts bss
    , afterTurn = Identity $
        let bs' = deref bs 0x082b
            bss = map BL.pack . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs'
        in getStmts (head bss)
    , interactiveGlobal = Identity $
        let bs' = deref bs 0x082d
        in interactive bs'
    , interactiveLocal = Identity $
        let bs' = BL.tail $ deref bs 0x0829
            bss = take numRooms $ map BL.pack . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs'
        in listArray (1, fromIntegral numRooms) $ map interactive bss
    , resetState = Identity $
        let minItem = BL.index bs 0x083a
            maxItem = BL.index bs 0x083b
            bs' = deref bs 0x0835
        in BL.take (fromIntegral $ maxItem - minItem) bs'
    , helpMap = Identity $
        let bs' = deref bs 0x0837
        in listArray (1, fromIntegral numRooms) $ BL.unpack bs'
    }
  where
    numRooms = 97

    (strings1, strings2) = loadMessages bs

    getStmts bs =
      let (_, stmts) = partitionEithers $ runGet `flip` bs $ untilEOF getStmt
      in stmts

    interactive bs =
        let bss = map BL.pack . takeWhile (not . null) . splitOn [0x00, 0x00] . BL.unpack $ bs
        in [ InputDispatch (BL.unpack input) (getStmts action)
           | bs <- bss
           , let (input, action) = BL.break (== 0x00) bs
           , not $ BL.null action
           , action <- pure $ BL.tail action
           ]

main :: IO ()
main = do
    Options{..} <- execParser optionsInfo
    createDirectoryIfMissing True outputPath

    bs <- BL.readFile inputPath
    bs <- pure $ BL.drop 2 $ bs

    let game = fromImage bs
        game' = pprGame game

    let write fileName = writeFile (outputPath </> fileName <.> "txt") . renderString .
          layoutPretty defaultLayoutOptions{ layoutPageWidth = Unbounded } . getConst

    write "dict" $ dict game'
    write "text1" $ msgs1 game'
    write "text2" $ msgs2 game'
    write "enter" $ enterRoom game'
    write "after" $ afterTurn game'
    write "interactive-global" $ interactiveGlobal game'
    write "interactive-local" $ interactiveLocal game'
    write "reset" $ resetState game'
    write "help" $ helpMap game'

options :: Parser Options
options = do
    inputPath <- strOption $ mconcat
        [ long "input"
        , short 'i'
        , metavar "FILENAME"
        , help "Memory dump"
        ]
    outputPath <- strOption $ mconcat
        [ long "output"
        , short 'o'
        , metavar "DIR"
        , help "Output directory"
        , value "."
        ]
    pure Options{..}

optionsInfo = info (options <**> helper) $ mconcat
    [ fullDesc
    , header "RatBC disassembler"
    ]
