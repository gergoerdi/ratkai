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
import RatBC.Picture

import Prettyprinter
import Prettyprinter.Render.String
import qualified Data.ByteString.Lazy as BL
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
    Options{..} <- execParser optionsInfo

    bs <- BL.readFile inputPath
    bs <- pure $ BL.drop 2 bs

    let game = fromImage bs
    game <- pure $ if block then mapStmts (\_ -> restoreBlocks) game else game

    createDirectoryIfMissing True outputPath
    writeTextFiles outputPath game

    let numPics = 54 -- TODO
        picBase = 0xa000
        picLen = (picRowstride `div` 8) * picHeight
        colorLen = picLen `div` 8
        picsLen = numPics * (picLen + colorLen)
    BL.writeFile (outputPath </> "pics-c64.bin") $ BL.take picsLen . BL.drop picBase $ bs


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
    block <- switch $ mconcat
        [ long "blocks"
        , short 'b'
        ]
    pure Options{..}

optionsInfo = info (options <**> helper) $ mconcat
    [ fullDesc
    , header "RatBC disassembler"
    ]
