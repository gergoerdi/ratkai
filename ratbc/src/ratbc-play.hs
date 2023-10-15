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

    bank1 <- readBin "text1"
    bank2 <- readBin "text2"
    dict <- readBin "dict"
    resetState <- readBin "reset"
    helpMap <- readBin "help"
    enterBC <- readBin "enter"
    afterBC <- readBin "after"
    globalBC <- readBin "interactive-global"
    localBCs <- readBin "interactive-local"
    let minItem = 120
        maxItem = 160 - 1
        startRoom = 1

    let parseWord = mkParser dict

    vars <- newArray (minBound, maxBound) 0x00
    forM_ (zip [minItem..] (BS.unpack resetState)) \(i, x) -> do
        writeArray vars i x
    writeArray vars playerLoc startRoom

    let loop moved = do
            -- lift dumpVars
            when moved $ lift do
                bc <- findByRoom enterBC
                runTerp bank2 bc
            mline <- getInputLine "RatBC> "
            case mline of
                Nothing -> return ()
                Just line -> do
                    let words = parseLine parseWord line
                    ((), Any moved) <- lift $ listen $ case words of
                        Nothing -> liftIO $ putStrLn "parse error"
                        Just [] -> pure ()
                        Just words -> do
                            localBC <- findByRoom localBCs
                            let mb_bc = msum
                                  [ findByInput words localBC
                                  , findByInput words globalBC
                                  ]
                            case mb_bc of
                                Just bc -> runTerp bank1 bc
                                Nothing -> runBuiltin words
                    loop moved
            -- line <- getInputLine "bosszu> "
            -- let tokens = tokenize dict line
            -- minput <- getInputLine "% "
            -- case minput of
            --     Nothing -> return ()
            --     Just "quit" -> return ()
            --     Just input -> do
            --         outputStrLn $ "Input was: " ++ input
            --         loop


    runEngine vars $ runInputT defaultSettings $ loop True
    pure ()

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