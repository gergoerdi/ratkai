{-# LANGUAGE ApplicativeDo, RecordWildCards, TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RatBC.Engine

import Data.ByteString
import qualified Data.ByteString as BS
import Options.Applicative
import System.Directory
import System.FilePath
import System.Console.Haskeline
import Data.Word

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

    print $ decodeZ . unpackZ $ text1

    -- let loop = do
    --         mline <- getInputLine "bosszu> "
    --         case mline of
    --             Nothing -> return ()
    --             Just line -> do
    --                 let tokens = tokenize dict line
    --                 loop
    --         -- line <- getInputLine "bosszu> "
    --         -- let tokens = tokenize dict line
    --         -- minput <- getInputLine "% "
    --         -- case minput of
    --         --     Nothing -> return ()
    --         --     Just "quit" -> return ()
    --         --     Just input -> do
    --         --         outputStrLn $ "Input was: " ++ input
    --         --         loop


    -- runInputT defaultSettings loop
    pure ()

tokenize :: ByteString -> String -> Maybe [Word8]
tokenize dict s = Nothing

-- parse :: ByteString -> String -> Maybe [Word8]
-- parse dict s = Nothing

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
