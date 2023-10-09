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
import RatBC.Game.ToHL

import Data.Functor.Const
import Control.Monad.Identity
import Data.Array (Array, listArray)
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

stripHomeLab :: Stmt -> Maybe Stmt
stripHomeLab = \case
    SetScreen{} -> Nothing
    SpriteOn{} -> Nothing
    SpriteOff{} -> Nothing
    Chime{} -> Nothing
    Sleep{} -> Nothing
    CopyProtection{} -> Nothing
    MachineCode addr _ -> Just $ MachineCode addr [0x60]
    -- If00 var body -> If00 var <$> let body' = mapMaybe stripHomeLab body in body' <$ guard (not . null $ body')
    -- IfFF var body -> IfFF var <$> let body' = mapMaybe stripHomeLab body in body' <$ guard (not . null $ body')
    s -> pure s

transformStmts :: ([Stmt] -> [Stmt]) -> (Game Identity -> Game Identity)
transformStmts f game = game
    { enterRoom = fmap (fmap f) $ enterRoom game
    , afterTurn = fmap f $ afterTurn game
    , interactiveGlobal = fmap (fmap (fmap f)) $ interactiveGlobal game
    , interactiveLocal = fmap (fmap (fmap (fmap f))) $ interactiveLocal game
    }

main :: IO ()
main = do
    opts@Options{..} <- execParser optionsInfo

    game <- loadTextFiles inputPath

    let f = mapMaybe stripHomeLab

    let game' = transformStmts f game

    createDirectoryIfMissing True outputPath
    writeTextFiles outputPath game'
    writeHLFiles outputPath game'

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
    pure Options{..}

optionsInfo = info (options <**> helper) $ mconcat
    [ fullDesc
    , header "RatBC disassembler"
    ]
