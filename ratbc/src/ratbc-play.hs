{-# LANGUAGE ApplicativeDo, RecordWildCards, TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RatBC.Engine

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Options.Applicative
import System.Directory
import System.FilePath
import System.Console.Haskeline
import Data.Word
import Data.Array.IO
import Data.Array
import Data.List.Split
import Text.Printf
import Control.Monad
import Control.Monad.RWS
import Data.Char
import Data.List (find)

data Options = Options
    { inputPath :: FilePath
    }

main :: IO ()
main = do
    Options{..} <- execParser optionsInfo

    let readBin fn = BS.readFile $ inputPath </> fn <.> "bin"

    text1 <- readBin "text1"
    text2 <- readBin "text2"
    dict <- readBin "dict"
    resetState <- readBin "reset"
    helpMap <- readBin "help"
    enterBC <- readBin "enter"
    afterBC <- readBin "after"
    globalBC <- readBin "interactive-global"
    localBC <- readBin "interactive-local"

    let parseWord = mkParser dict

    vars <- newArray (minBound, maxBound) 0x00
    dumpVars vars

    putStrLn $ decodeZ . fst . unpackZ . findZ text1 $ 2

    let loop = do
            mline <- getInputLine "bosszu> "
            case mline of
                Nothing -> return ()
                Just line -> do
                    liftIO . print $ parseLine parseWord line
                    loop
            -- line <- getInputLine "bosszu> "
            -- let tokens = tokenize dict line
            -- minput <- getInputLine "% "
            -- case minput of
            --     Nothing -> return ()
            --     Just "quit" -> return ()
            --     Just input -> do
            --         outputStrLn $ "Input was: " ++ input
            --         loop


    runInputT defaultSettings loop
    pure ()

dumpVars :: IOArray Word8 Word8 -> IO ()
dumpVars vars = do
    vars <- freeze vars :: IO (Array Word8 Word8)
    forM_ (chunksOf 16 $ elems vars) \chunk -> do
        mapM_ (printf "%02x ") chunk
        printf "\n"

tokenize :: String -> [String]
tokenize = split (dropBlanks $ dropDelims $ oneOf [' ']) . map toUpper

mkParser :: ByteString -> String -> Maybe Word8
mkParser bs = \input -> snd <$> find (matchWord input . fst) dict
  where
    dict = unpackWords bs
    matchWord input word = case (input, word) of
        (_, []) -> True
        ([], ' ':w) -> matchWord [] w
        (i:is, c:cs) | i == c -> matchWord is cs
        _ -> False

parseLine :: (String -> Maybe Word8) -> String -> Maybe [Word8]
parseLine parseWord = fmap (filter (/= 100)) . mapM parseWord . tokenize

matchWord :: ByteString -> Int -> String -> Bool
matchWord bs ptr s = undefined

options :: Parser Options
options = do
    inputPath <- strOption $ mconcat
        [ long "input"
        , short 'i'
        , metavar "FILENAME"
        , help "Memory dump"
        ]
    pure Options{..}

optionsInfo = info (options <**> helper) $ mconcat
    [ fullDesc
    , header "RatBC player"
    ]
