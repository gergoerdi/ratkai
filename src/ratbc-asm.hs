{-# LANGUAGE ApplicativeDo, RecordWildCards, TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RatBC.Utils
import RatBC.Syntax
import RatBC.Words
import RatBC.Text
import RatBC.Pretty
import RatBC.Game
import RatBC.Game.Text
import RatBC.Commodore64.Binary as C64

import RatBC.Map
import Data.GraphViz
import Data.GraphViz.Commands.IO

import Options.Applicative
import System.Directory
import System.FilePath

data Options = Options
    { inputPath :: FilePath
    , outputPath :: FilePath
    , block :: Bool
    }

main :: IO ()
main = do
    opts@Options{..} <- execParser optionsInfo

    game <- loadTextFiles inputPath
    game <- pure $ if block then mapStmts (\_ -> C64.restoreBlocks) game else game
    createDirectoryIfMissing True outputPath
    writeTextFiles outputPath game

    writeDotFile (outputPath </> "map.dot") $ roomDot game

options :: Parser Options
options = do
    inputPath <- strOption $ mconcat
        [ long "input"
        , short 'i'
        , metavar "DIR"
        , help "Input directory"
        ]
    outputPath <- strOption $ mconcat
        [ long "output"
        , short 'o'
        , metavar "DIR"
        , help "Output directory"
        , value "."
        ]
    block <- switch $ mconcat
        [ long "blocks"
        , short 'b'
        ]
    pure Options{..}

optionsInfo = info (options <**> helper) $ mconcat
    [ fullDesc
    , header "RatBC assembler"
    ]
