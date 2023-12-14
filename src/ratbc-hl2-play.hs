{-# LANGUAGE ApplicativeDo, RecordWildCards, TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns, TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import RatBC.Game (Bank(..))
import RatBC.Engine
import RatBC.Engine.GameLoop
import RatBC.Engine.ZSCII

import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Options.Applicative
import System.Directory
import System.FilePath
import System.Console.Haskeline
import Data.Word
import Data.Array.IO
import Data.Array
import Data.Char
import Data.List (find)
import System.IO
import Data.Foldable (traverse_)

data Options = Options
    { inputPath :: FilePath
    , transcriptPath :: Maybe FilePath
    , debug :: Bool
    }

newtype ZSCIIMessages m a = ZSCIIMessages{ unZSCIIMessages :: ReaderT (ByteString, ByteString) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance (MonadIO m) => MonadMessage (ZSCIIMessages m) where
    printMessage bank msg = ZSCIIMessages do
        (bank1, bank2) <- ask
        printlnZ (case bank of Bank1 -> bank1; Bank2 -> bank2) msg

runZSCIIMessage :: ByteString -> ByteString -> ZSCIIMessages m a -> m a
runZSCIIMessage bank1 bank2 act = runReaderT (unZSCIIMessages act) (bank1, bank2)

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

    let parseWord = mkParser $ unpackDict dict

    vars <- newArray (minBound, maxBound) 0x00
    forM_ (zip [minItem..] (BS.unpack resetState)) \(i, x) -> do
        writeArray vars i x
    writeArray vars playerLoc startRoom

    initialTranscript <- case transcriptPath of
        Nothing -> pure []
        Just fileName -> lines <$> readFile' fileName
    let withAppendMaybe maybePath body = case maybePath of
            Nothing -> body $ \_ -> pure ()
            Just fileName -> withFile fileName AppendMode \h -> body $ hPutStrLn h

    withAppendMaybe transcriptPath \appendLine -> do
        void $ runInputT defaultSettings $ runZSCIIMessage bank1 bank2 $ runEngine minItem maxItem vars helpMap do
            runGame
                enterBC afterBC localBCs globalBC parseWord
                (liftIO . appendLine)
                (lift . getInputLine)
                initialTranscript

options :: Parser Options
options = do
    inputPath <- strOption $ mconcat
        [ long "input"
        , short 'i'
        , metavar "DIRECTORY"
        , help "HL2 datafile directory"
        ]
    transcriptPath <- optional $ strOption $ mconcat
        [ long "transcript"
        , short 'l'
        , metavar "FILENAME"
        , help "Transcript"
        ]
    debug <- switch $ mconcat
        [ long "debug"
        , short 'd'
        ]
    pure Options{..}

optionsInfo = info (options <**> helper) $ mconcat
    [ fullDesc
    , header "RatBC player"
    ]
