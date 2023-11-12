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

parseGame :: Game (Const String) -> Game Identity
parseGame game@Game{..} = game
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

    parseMessages s = let xs = read s
      in array (1, fromIntegral $ length xs) xs

    parseWords = M.fromList . read

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
