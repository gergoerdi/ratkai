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
import Data.Array (Array, array, listArray, range, assocs)
import Data.List (stripPrefix, lookup)
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
    forM_ (assocs $ sprites game) \(i, bs) -> unless (BL.null bs) do
        bin (printf "sprite-%02d" i) bs
  where
    game' = pprGame game
    write fileName = writeFile (outputPath </> fileName <.> "txt") . renderString .
      layoutPretty defaultLayoutOptions{ layoutPageWidth = Unbounded } . getConst
    bin fileName = BL.writeFile (outputPath </> fileName <.> "bin")

parseGame :: Game (Const String) -> Game Identity
parseGame game@Game{..} = game
    { msgs1 = Identity . parseMessages "text1" . getConst $ msgs1
    , msgs2 = Identity . parseMessages "text2" . getConst $ msgs2
    , dict = Identity . parseWords . getConst $ dict
    , enterRoom = Identity . fromList . parse "enter" . getConst $ enterRoom
    , afterTurn = Identity . parse "after" . getConst $ afterTurn
    , interactiveGlobal = Identity . parse "interactive-global" . getConst $ interactiveGlobal
    , interactiveLocal =  Identity . fromList . parse "interactive-local" . getConst $ interactiveLocal
    , resetState = Identity . BL.pack . read . getConst $ resetState
    , helpMap = Identity . fromList . read . getConst $ helpMap
    }
  where
    fromList :: [a] -> Array Word8 a
    fromList xs = listArray (1, fromIntegral $ length xs) xs

    parse :: (Read a, Show a) => String -> String -> a
    parse tag s = case reads s of
        [(x, s')] | all isSpace s' -> x
        ps -> error $ printf "Parse error in '%s'" tag

    parseMessages tag s = let xs = parse tag s
      in array (1, fromIntegral $ length xs) xs

    parseWords = M.fromList . parse "dict"

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
      <*> sprites
    return $ parseGame game0
  where
    sprites = do
        files <- listDirectory inputPath
        let spriteFiles =
              [ (i, fp)
              | fp <- files
              , takeExtension fp == "bin"
              , Just s <- pure $ stripPrefix "sprite-" (takeBaseName fp)
              , let i = read s
              ]
        let bounds = (minimum . map fst $ spriteFiles, maximum . map fst $ spriteFiles)
        listArray bounds <$> forM (range bounds) \i ->
            case lookup i spriteFiles of
                Nothing -> mempty
                Just fp -> BL.readFile fp
