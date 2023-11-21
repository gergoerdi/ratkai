{-# LANGUAGE ApplicativeDo, RecordWildCards, TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module RatBC.Game.Text where

import RatBC.Utils
import RatBC.Syntax
import RatBC.Words
import RatBC.Text
import RatBC.Game
import RatBC.Pretty
import RatBC.Game.Parse

import Text.Read
import Control.Monad.Identity
import Prettyprinter
import Prettyprinter.Render.String
import qualified Data.ByteString.Lazy as BL
import Options.Applicative
import System.Directory
import System.FilePath
import qualified Data.Map as M
import Data.Word
import Data.Array (Array, array, listArray)
import Data.List.Split
import Data.Char (isSpace)
import Text.Printf

writeTextFiles :: FilePath -> Game Identity -> IO ()
writeTextFiles outputPath game = do
    write "dict" $ dict game'
    write "text1" $ msgs1 game'
    write "text2" $ msgs2 game'
    write "enter" $ enterRoom game'
    write "after" $ afterTurn game'
    write "interactive-global" $ interactiveGlobal game'
    write "interactive-local" $ interactiveLocal game'
    write "reset" $ resetState game'
    write "help" $ helpMap game'
    bin "charset" $ charSet game
  where
    game' = pprGame game
    write fileName = writeFile (outputPath </> fileName <.> "txt") . renderString .
      layoutPretty defaultLayoutOptions{ layoutPageWidth = Unbounded } . getConst
    bin fileName = BL.writeFile (outputPath </> fileName <.> "bin")

readGame :: Game (Const String) -> Game Identity
readGame game@Game{..} = game
    { msgs1 = Identity . readMessages "text1" . getConst $ msgs1
    , msgs2 = Identity . readMessages "text2" . getConst $ msgs2
    , dict = Identity . readWords . getConst $ dict
    , enterRoom = Identity . fromList . readFrom "enter" . getConst $ enterRoom
    , afterTurn = Identity . readFrom "after" . getConst $ afterTurn
    , interactiveGlobal = Identity . readFrom "interactive-global" . getConst $ interactiveGlobal
    , interactiveLocal =  Identity . fromList . readFrom "interactive-local" . getConst $ interactiveLocal
    , resetState = Identity . BL.pack . read . getConst $ resetState
    , helpMap = Identity . fromList . read . getConst $ helpMap
    }
  where
    fromList :: [a] -> Array Word8 a
    fromList xs = listArray (1, fromIntegral $ length xs) xs

    readFrom :: (Read a, Show a) => String -> String -> a
    readFrom tag s = case reads s of
        [(x, s')] | all isSpace s' -> x
        ps -> error $ printf "Read error in '%s'" tag

    readMessages tag s = let xs = readFrom tag s
      in array (1, fromIntegral $ length xs) xs

    readWords = M.fromList . readFrom "dict"

removeComments :: String -> String
removeComments = unlines . map removeComment . lines
  where
    removeComment s = case endBy "-- " s of
        [] -> ""
        (s:ss) -> s

loadTextFiles :: FilePath -> IO (Game Identity)
loadTextFiles inputPath = do
    let file fileName = Const . removeComments <$> readFile (inputPath </> fileName <.> "txt")
        bin fileName =  BL.readFile (inputPath </> fileName <.> "bin")

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
      <*> pure 120 -- TODO: minItem
      <*> pure 160 -- TODO: maxItem
      <*> pure 1 -- TODO: startRoom
      <*> bin "charset"
    return $ parseGame game0
