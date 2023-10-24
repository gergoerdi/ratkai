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
import RatBC.HomeLab2.Strip

import Options.Applicative
import Control.Monad.State
import Data.Functor.Const
import Control.Monad.Identity
import Data.Array (Array, array, listArray, elems, assocs, bounds, (!), indices)
import Data.Ix (Ix(..))
import Prettyprinter
import Prettyprinter.Render.String
import Data.String
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.List.Split
import Data.List (nub, sort, (\\))
import Text.Printf
import Data.Word
import Data.Maybe
import Data.Either
import Data.Char
import System.Directory
import System.FilePath
import Data.Foldable (toList)
import RatBC.Map
import Data.GraphViz
import Data.GraphViz.Commands.IO

data Options = Options
    { inputPath :: FilePath
    , outputPath :: FilePath
    , block :: Bool
    }

reflowMessages :: Game Identity -> Game Identity
reflowMessages game@Game{..} = game
    { msgs1 = fmap (fmap $ wrapWords 40) msgs1
    , msgs2 = fmap (fmap $ wrapWords 40) msgs2
    }

main :: IO ()
main = do
    opts@Options{..} <- execParser optionsInfo

    game <- loadTextFiles inputPath
    game <- pure $ if block then mapStmts (\_ -> restoreBlocks) game else game
    game <- pure $ stripGame game

    createDirectoryIfMissing True outputPath
    writeTextFiles outputPath game

    game <- pure $ reflowMessages game
    writeHLFiles outputPath game
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
    , header "RatBC HomeLab2 compiler"
    ]
