{-# LANGUAGE ApplicativeDo, RecordWildCards, TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RatBC.Syntax
import RatBC.Map
import RatBC.Game
import RatBC.Game.Text

import RatBC.Strip
import RatBC.TVC.Binary as TVC
import RatBC.TVC.Picture as TVC

import qualified Data.ByteString as BS
import Options.Applicative
import Control.Monad.Identity
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
    game <- pure $ if block then mapStmts (\_ -> restoreBlocks) game else game
    game <- pure $ stripGame True game

    createDirectoryIfMissing True outputPath
    writeTextFiles outputPath game

    game <- pure $ reflowMessages False 31 game
    TVC.writeFiles outputPath game

    pics <- BS.readFile (inputPath </> "pics-c64.bin")
    let numPics = 54 -- TODO
    BS.writeFile (outputPath </> "pics-tvc.bin") $ mconcat
        [ TVC.fromC64 bs
        | i <- take numPics [0..]
        , let size = 450
        , let addr = i * size
        , let bs = BS.take size . BS.drop addr $ pics
        ]

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
    , header "RatBC TVC compiler"
    ]
