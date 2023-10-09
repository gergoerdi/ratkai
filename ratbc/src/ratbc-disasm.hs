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
import RatBC.Game.FromImage
import RatBC.Game.Text

import Prettyprinter
import Prettyprinter.Render.String
import qualified Data.ByteString.Lazy as BL
import Options.Applicative
import System.Directory
import System.FilePath

data Options = Options
    { inputPath :: FilePath
    , outputPath :: FilePath
    }

main :: IO ()
main = do
    Options{..} <- execParser optionsInfo

    bs <- BL.readFile inputPath
    bs <- pure $ BL.drop 2 bs

    let game = fromImage bs
    createDirectoryIfMissing True outputPath
    writeTextFiles outputPath game

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
