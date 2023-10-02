{-# LANGUAGE ApplicativeDo, RecordWildCards, TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
module Main where

import RatBC.Syntax
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
    If00 var body -> If00 var <$> let body' = mapMaybe stripHomeLab body in body' <$ guard (not . null $ body')
    IfFF var body -> IfFF var <$> let body' = mapMaybe stripHomeLab body in body' <$ guard (not . null $ body')
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
    bs <- pure $ BL.drop startAddr . BL.drop 2 $ bs

    let test bs = do
            let stmts = runGet `flip` bs $ untilEOF (get @Stmt)
            let stmts' = mapMaybe stripHomeLab stmts
                bs' = runPut $ mapM_ put stmts'
            unless (bs == bs') do
                mapM_ print stmts
                print $ BL.unpack bs
                print $ BL.unpack bs'
            return $ BL.length bs'

    -- let bss = map BL.pack . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs
    -- lengths <- forM (zip [(1 :: Word8)..] $ take 96 bss) \(room, bs) -> do
    --     printf "ROOM %d {\n" room
    --     len <- test bs
    --     printf "}\n\n"
    --     return $ len + 3

    -- print $ sum lengths

    let bss = map BL.pack . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs
    print =<< test (head bss)
  where
    -- -- szobaba belepve
    -- startAddr = 0x402a

    -- minden lepes utan
    startAddr = 0x5ca1

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
