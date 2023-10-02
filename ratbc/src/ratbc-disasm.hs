{-# LANGUAGE ApplicativeDo, RecordWildCards, TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns #-}
module Main where

import RatBC.Syntax
import RatBC.Words
import RatBC.Text

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Options.Applicative
import Control.Monad
import Data.List.Split
import Text.Printf
import Data.Word
import Data.Maybe
import Data.Either

data Options = Options
    { inputPath :: FilePath
    , outputPath :: Maybe FilePath
    }

stripHomeLab :: Stmt -> Maybe Stmt
stripHomeLab = \case
    SetScreen{} -> Nothing
    SpriteOn{} -> Nothing
    SpriteOff{} -> Nothing
    Chime{} -> Nothing
    Sleep{} -> Nothing
    CopyProtection{} -> Nothing
    -- If00 var body -> If00 var <$> let body' = mapMaybe stripHomeLab body in body' <$ guard (not . null $ body')
    -- IfFF var body -> IfFF var <$> let body' = mapMaybe stripHomeLab body in body' <$ guard (not . null $ body')
    s -> pure s

isHomeLab :: Stmt -> Bool
isHomeLab = \case
    SetScreen{} -> False
    SpriteOn{} -> False
    SpriteOff{} -> False
    Chime{} -> False
    Sleep{} -> False
    _ -> True

main :: IO ()
main = do
    Options{..} <- execParser optionsInfo
    bs <- BL.readFile inputPath
    bs <- pure $ BL.drop 2 $ bs

    let (strings1, strings2) = loadMessages bs

    let test bs = do
            let (_, stmts) = partitionEithers $ runGet `flip` bs $ untilEOF getStmt
            let stmts' = mapMaybe stripHomeLab stmts
                bs' = runPut $ mapM_ put stmts'
            unless (False && bs == bs') do
                mapM_ print stmts
                print $ BL.unpack bs
                print $ BL.unpack bs'
            return $ BL.length bs'

    -- BC_ENTER_ROOM
    when False do
        bs <- pure $ BL.drop 0x402a bs
        let bss = map BL.pack . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs
        lengths <- forM (zip [(1 :: Word8)..] $ take 96 bss) \(room, bs) -> do
            printf "ROOM %d {\n" room
            len <- test bs
            printf "}\n\n"
            return $ len + 3

        print $ sum lengths

    -- BC_AFTER
    when False do
        bs <- pure $ BL.drop 0x5ca1 bs
        let bss = map BL.pack . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs
        print =<< test (head bss)

    -- BC_INTERACTIVE
    when False do
        bs <- pure $ BL.drop 0x5d3b bs
        let bss = map BL.pack . takeWhile (not . null) . splitOn [0x00, 0x00] . BL.unpack $ bs
        lengths <- forM bss \bs -> do
            let (input, action) = BL.break (== 0x00) bs
            if BL.null action then return (BL.length input + 1) else do
                len <- test (BL.tail action)
                return $ BL.length input + len + 2
        print $ sum lengths

    -- BC_INTERACTIVE_LOCAL
    when False do
        bs <- pure $ BL.drop 0x466a bs
        let bss = map BL.pack . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs
        lengths <- forM (zip [(1 :: Word8)..] $ take 96 bss) \(room, bs) -> do
            printf "ROOM %d {\n" room
            let bss = map BL.pack . takeWhile (not . null) . splitOn [0x00, 0x00] . BL.unpack $ bs
            len <- fmap sum $ forM bss \bs -> do
                let (input, action) = BL.break (== 0x00) bs
                if BL.null action then return (BL.length input + 1) else do
                    len <- test (BL.tail action)
                    return $ BL.length input + len + 2
            printf "}\n\n"
            return $ len + 3
        print $ sum lengths

    -- Dictionary
    when False do
        let dict = loadWords bs
        print dict

    when True do
        mapM_ putStrLn strings1
        mapM_ putStrLn strings2

options :: Parser Options
options = do
    inputPath <- strOption $ mconcat
        [ long "input"
        , short 'i'
        , metavar "FILENAME"
        , help "Memory dump"
        ]
    outputPath <- optional $ strOption $ mconcat
        [ long "output"
        , short 'o'
        , metavar "FILENAME"
        , help "Output text file"
        ]
    pure Options{..}

optionsInfo = info (options <**> helper) $ mconcat
    [ fullDesc
    , header "RatBC disassembler"
    ]
