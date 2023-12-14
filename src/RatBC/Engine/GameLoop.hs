{-# LANGUAGE ApplicativeDo, RecordWildCards, TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns, TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RatBC.Engine.GameLoop where

import RatBC.Game (Bank(..))
import RatBC.Engine

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.List.Split
import Control.Monad
import Control.Monad.Writer
import Data.Foldable (traverse_)
import Data.Char (toUpper)
import Data.List (find)

runGame
    :: forall m. (MonadIO m, MonadMessage m)
    => ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> (String -> Maybe Word8)
    -> (String -> m ())
    -> (String -> m (Maybe String))
    -> [String]
    -> Engine m ()
runGame enterBC afterBC localBCs globalBC parseWord appendLine getInputLine lines = loop lines True
  where
    runInput :: String -> Engine m Bool
    runInput line = do
        let words = parseLine parseWord line
        ((), Any moved) <- listen $ case words of
            Nothing -> printMessage Bank1 1
            Just [] -> pure ()
            Just words -> do
                localBC <- findByRoom localBCs
                let mb_bc = msum
                      [ findByInput words localBC
                      , findByInput words globalBC
                      ]
                case mb_bc of
                    Just bc -> runTerp Bank1 bc
                    Nothing -> runBuiltin words
        runTerp Bank2 afterBC
        pure moved

    loop :: [String] -> Bool -> Engine m ()
    loop lines moved = do
        -- when debug dumpVars
        when moved do
            bc <- findByRoom enterBC
            runTerp Bank2 bc
            loc <- getVar' playerLoc
            items <- getItems
            let itemsHere = map fst . filter ((loc ==) . snd) $ items
            unless (null itemsHere) $ do
                printMessage Bank1 11
                mapM_ (printMessage Bank2) itemsHere
        (mline, lines') <- case lines of
            (line:lines') -> do
                liftIO $ putStrLn $ "RatBC> " <> line
                pure (Just line, lines')
            [] -> do
                mline <- lift $ getInputLine "RatBC> "
                lift $ traverse_ appendLine mline
                pure (mline, [])
        case mline of
            Nothing -> return ()
            Just line -> do
                liftIO $ putStrLn ""
                moved <- runInput line
                loop lines' moved

tokenize :: String -> [String]
tokenize = split (dropBlanks $ dropDelims $ oneOf [' '])

parseLine :: (String -> Maybe Word8) -> String -> Maybe [Word8]
parseLine parseWord = fmap (filter (/= 100)) . mapM parseWord . tokenize

mkParser :: [(String, Word8)] -> String -> Maybe Word8
mkParser dict = \input -> snd <$> find (matchWord (map toUpper input) . fst) dict
  where
    matchWord input word = case (input, word) of
        (_, []) -> True
        ([], ' ':w) -> matchWord [] w
        (i:is, c:cs) | i == c -> matchWord is cs
        _ -> False
