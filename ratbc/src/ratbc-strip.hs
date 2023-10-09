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

removeComments :: String -> String
removeComments = unlines . map (head . endBy "-- ") . lines

parseGame :: Game (Const String) -> Game Identity
parseGame Game{..} = Game
    { msgs1 = Identity . parseMessages . getConst $ msgs1
    , msgs2 = Identity . parseMessages . getConst $ msgs2
    , dict = Identity . parseWords . getConst $ dict
    , enterRoom = Identity . fromList . read . getConst $ enterRoom
    , afterTurn = Identity . read . getConst $ afterTurn
    , interactiveGlobal = Identity . read . getConst $ interactiveGlobal
    , interactiveLocal =  Identity . fromList . read . getConst $ interactiveLocal
    , resetState = Identity . BL.pack . read . getConst $ resetState
    , helpMap = Identity . fromList . read . getConst $ helpMap
    }
  where
    fromList :: [a] -> Array Word8 a
    fromList xs = listArray (1, fromIntegral $ length xs) xs

    parseMessages s = listArray (1, fromIntegral $ length ss) $ map (dropWhile (== ' ') . tail . snd . break (== ':')) ss
      where
        ss = lines s

    parseWords = M.fromList . read

main :: IO ()
main = do
    opts@Options{..} <- execParser optionsInfo

    let file fileName = Const . removeComments <$> readFile (inputPath </> fileName <.> "txt")

    game0 <- Game
      <$> file "text1"
      <*> file "text2"
      <*> file "dict"
      <*> file "enter"
      <*> file "after"
      <*> file "interactive-global"
      <*> file "interactive-local"
      <*> file "reset"
      <*> file "help"

    let game = parseGame game0

    let f = mapMaybe stripHomeLab

    let game' = game
          { enterRoom = fmap (fmap f) $ enterRoom game
          }

    createDirectoryIfMissing True outputPath
    writeTextFiles outputPath game'

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
