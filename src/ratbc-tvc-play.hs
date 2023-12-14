{-# LANGUAGE ApplicativeDo, RecordWildCards, TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns, TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import RatBC.Game
import RatBC.Game.Text
import RatBC.TVC.Binary
import RatBC.TVC.Text
import RatBC.TVC.Strip

import RatBC.Engine
import RatBC.Engine.GameLoop

import Data.Bifunctor
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Options.Applicative
import System.Directory
import System.FilePath
import System.Console.Haskeline
import Data.Word
import Data.Array.IO
import Data.Array
import Data.Char
import Data.List (find)
import Data.List.Split
import System.IO
import Data.Foldable (traverse_)

data Options = Options
    { inputPath :: FilePath
    , transcriptPath :: Maybe FilePath
    , debug :: Bool
    }

newtype Messages m a = Messages{ unMessages :: ReaderT (ByteString, ByteString) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance (MonadIO m) => MonadMessage (Messages m) where
    printMessage bank msg = Messages do
        (bank1, bank2) <- ask
        let bs = case bank of Bank1 -> bank1; Bank2 -> bank2
            (n, bs') = leap bs msg
        liftIO $ mapM_ putStrLn . reflow $ fromTVCString . BS.take (fromIntegral $ n - 1) $ bs'
        liftIO $ putStrLn ""
      where
          reflow :: String -> [String]
          reflow = concatMap (chunksOf 31) . split (dropDelims . onSublist $ "\n")

fromTVCString :: ByteString -> String
fromTVCString = map fromTVCChar . BS.unpack

runMessages :: ByteString -> ByteString -> Messages m a -> m a
runMessages bank1 bank2 act = runReaderT (unMessages act) (bank1, bank2)

main :: IO ()
main = do
    Options{..} <- execParser optionsInfo

    assets <- loadTextFiles inputPath
    let assets' = mapGameF (first BL.toStrict) . assemble . reflowMessages 31 . preprocessGame $ assets
    let asset :: forall a x. (Game (Const ByteString) -> Const a x) -> a
        asset sel = getConst $ sel assets'

    let helps = asset helpMap
        enterBC = asset enterRoom
        afterBC = asset afterTurn
        localBC = asset interactiveLocal
        globalBC = asset interactiveGlobal

    let parseWord = mkParser (unpackDict $ asset dict)
    when debug $ mapM_ print $ unpackDict $ asset dict

    vars <- newArray (minBound, maxBound) 0x00
    forM_ (zip [minItem assets'..] (BS.unpack $ asset resetState)) \(i, x) -> do
        writeArray vars i x
    writeArray vars playerLoc (startRoom assets')

    initialTranscript <- case transcriptPath of
        Nothing -> pure []
        Just fileName -> lines <$> readFile' fileName
    let withAppendMaybe maybePath body = case maybePath of
            Nothing -> body $ \_ -> pure ()
            Just fileName -> withFile fileName AppendMode \h -> body $ hPutStrLn h

    withAppendMaybe transcriptPath \appendLine -> do
        void $ runInputT defaultSettings $ runMessages (asset msgs1) (asset msgs2) $ runEngine False (minItem assets') (maxItem assets' - 1) vars helps do
            runGame
                enterBC afterBC localBC globalBC parseWord
                (liftIO . appendLine)
                (lift . getInputLine)
                initialTranscript

unpackDict :: ByteString -> [(String, Word8)]
unpackDict bs
  | BS.head bs == 0xff = []
  | otherwise = (fromTVCString w, b) : unpackDict bs'
  where
    w = BS.take 5 bs
    Just (b, bs') = BS.uncons . BS.drop 5 $ bs

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
