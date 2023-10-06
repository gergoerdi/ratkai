{-# LANGUAGE ApplicativeDo, RecordWildCards, TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RatBC.Utils
import RatBC.Syntax
import RatBC.Words
import RatBC.Text
import RatBC.Pretty

import Data.Array (listArray)
import Prettyprinter
import Prettyprinter.Render.String
import Data.String
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Options.Applicative
import Control.Monad
import Data.List.Split
import Text.Printf
import Data.Word
import Data.Maybe
import Data.Either
import Data.Char
import System.Directory
import System.FilePath

data Options = Options
    { inputPath :: FilePath
    , outputPath :: FilePath
    }

stripHomeLab :: Stmt -> Maybe Stmt
stripHomeLab = \case
    SetScreen{} -> Nothing
    SpriteOn{} -> Nothing
    SpriteOff{} -> Nothing
    Chime{} -> Nothing
    Sleep{} -> Nothing
    CopyProtection{} -> Nothing
    MachineCode addr _ -> Just $ MachineCode addr [0x60]
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
    createDirectoryIfMissing True outputPath

    bs <- BL.readFile inputPath
    bs <- pure $ BL.drop 2 $ bs

    let (strings1, strings2) = loadMessages bs
    let msgs1 = listArray (1, fromIntegral $ length strings1) strings1
        msgs2 = listArray (1, fromIntegral $ length strings2) strings2
    strings1 <- pure $ map (map toUpper) strings1
    strings2 <- pure $ map (map toUpper) strings2

    let dict = loadWords bs
    printf "Loaded %d words\n" $ M.size dict
    -- print $ M.keys dict
    let pprWords bs = hsep
          [ fromString $ head words
          | b <- BL.unpack bs
          , let words = fromMaybe [printf "|%02x|" b] $ M.lookup b dict
          ]

    let test bs = do
            let (_, stmts) = partitionEithers $ runGet `flip` bs $ untilEOF getStmt
            let stmts' = mapMaybe stripHomeLab stmts
                bs' = runPut $ mapM_ put stmts'
            -- unless (False && bs == bs') do
            --     mapM_ print stmts'
            --     print $ BL.unpack bs
            --     print $ BL.unpack bs'
            return (bs', stmts)

    -- BC_ENTER_ROOM
    when True do
        bs <- pure $ deref bs 0x0827
        let bss = map BL.pack . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs
        (bss', docs) <- fmap unzip $ forM (zip [(1 :: Word8)..] $ take 96 bss) \(room, bs) -> do
            (bs', stmts) <- test bs
            let doc = vsep ["ROOM" <+> viaShow room, pprStmts msgs2 stmts]
            return (bs', doc)
        let bs' = BL.intercalate (BL.pack [0x00, 0x00, 0x00]) bss'
            doc = vsep docs <> line
        BL.writeFile (outputPath </> "enter.bc") bs'
        writeFile (outputPath </> "enter.txt") $ show doc
        print $ BL.length bs'

    -- BC_AFTER
    when True do
        bs <- pure $ deref bs 0x082b
        let bss = map BL.pack . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs
        (bs', stmts) <- test (head bss)
        let doc = pprStmts msgs1 stmts
        BL.writeFile (outputPath </> "after.bc") bs'
        writeFile (outputPath </> "after.txt") $ show doc
        print $ BL.length bs'
        pure ()

    let interactive bs = do
            let bss = map BL.pack . takeWhile (not . null) . splitOn [0x00, 0x00] . BL.unpack $ bs
            (bss', docs) <- fmap unzip $ forM bss \bs -> do
                let (input, action) = BL.break (== 0x00) bs
                if BL.null action then return (input <> BL.singleton 0x00, mempty) else do
                    action <- pure $ BL.tail action
                    (bs', stmts) <- test action
                    let doc = vcat
                          [ pprBytes input <+> "--" <+> pprWords input
                          , pprStmts msgs1 stmts
                          ]
                    let bs'' = input <> BL.singleton 0x00 <> bs' <> BL.pack [0x00, 0x00]
                    return (bs'', doc)
            pure (mconcat bss', vsep docs)

    -- BC_INTERACTIVE
    when True do
        bs <- pure $ deref bs 0x082d
        let bss = map BL.pack . takeWhile (not . null) . splitOn [0x00, 0x00] . BL.unpack $ bs
        (bs', doc) <- interactive bs
        BL.writeFile (outputPath </> "interactive-global.bc") bs'
        writeFile (outputPath </> "interactive-global.txt") $ show doc
        print $ BL.length bs'
        pure ()

    -- BC_INTERACTIVE_LOCAL
    when True do
        bs <- pure $ BL.tail $ deref bs 0x0829
        let bss = map BL.pack . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs
        (bss', docs) <- fmap unzip $ forM (zip [(1 :: Word8)..] $ take 96 bss) \(room, bs) -> do
            (bs', doc) <- interactive bs
            let doc' = vsep ["ROOM" <+> viaShow room, doc]
            pure (bs', doc')
        let bs' = BL.intercalate (BL.pack [0x00, 0x00, 0x00]) bss'
        let doc = vsep docs <> line
        print $ BL.length bs'
        BL.writeFile (outputPath </> "interactive-local.bc") bs'
        writeFile (outputPath </> "interactive-local.txt") $ show doc
        pure ()

    -- Dictionary
    when True do
        let bss = [ zscii word | words <- M.elems dict, let word = map toUpper $ mconcat words ]
        let bs = mconcat bss
        BL.writeFile (outputPath </> "dictionary.words") bs
        print $ BL.length bs
        -- mapM_ (putStrLn . concat) $ M.elems dict

    when True do
        let strings1' = {-delete [158, 159, 192, 196] -} strings1
        let bs1 = mconcat $ map zscii strings1'
        BL.writeFile (outputPath </> "text1.zscii") bs1
        print $ BL.length bs1

        let strings2' = {-delete [1, 155, 156, 157, 158, 159, 160, 161, 191, 192] -} strings2
        let bs2 = mconcat $ map zscii strings2'
        BL.writeFile (outputPath </> "text2.zscii") bs2
        print $ BL.length bs2

        -- mapM_ (\(i, s) -> printf "%03d: %s\n" i s) $ zip [(1::Int)..] strings1'
        -- mapM_ (\(i, s) -> printf "%03d: %s\n" i s) $ zip [(1::Int)..] strings2'

    when True do
        let minItem = BL.index bs 0x083a
            maxItem = BL.index bs 0x083b
        bs <- pure $ deref bs 0x0837
        let bs' = BL.take (fromIntegral $ maxItem - minItem) bs
        BL.writeFile (outputPath </> "reset.dat") bs'
        print $ BL.length bs'

    when True do
        bs <- pure $ deref bs 0x0835
        let bs' = BL.take 96 bs
        BL.writeFile (outputPath </> "help.dat") bs'
        print $ BL.length bs'

delete :: [Int] -> [a] -> [a]
delete is = map snd . filter (\(i, _) -> i `notElem` is) . zip [1..]

zscii :: String -> BL.ByteString
zscii = go . concatMap val
  where
    go = \case
        (x:y:z:s@(_:_)) -> cons (x, y, z) s
        [x, y, z] -> end (x, y, z)
        [x, y] -> end (x, y, 0)
        [x] -> end (x, 0, 0)
        [] -> error "zscii: empty string"
        -- [] -> end (0, 0, 0)

    val :: Char -> [Word8]
    -- val c | 'A' <= c, c <= 'Z' = [fromIntegral $ ord c - ord 'A']
    --       | '0' <= c, c <= '9' = []
    --       | c == ' ' = [26]
    --       | c == '.' = [27]
    --       | c == '!' = [26] -- XXX
    --       | c == '?' = [26]
    --       | c == '\'' = [26] -- XXX
    --       | c == ':' = [29]
    --       | c == '-' = [26] -- XXX
    --       | c == ',' = [30]
    --       | c == '|' = [] -- TODO: these should have been filtered out earlier...
    --       | otherwise = error $ show c
    --       -- | otherwise = []
    val c | 'A' <= c, c <= 'Z' = [fromIntegral $ 6 + ord c - ord 'A']
          | '0' <= c, c <= '9' = [1, fromIntegral $ 6 + ord c - ord '0']
          | c == ' ' = [1, 6 + 10]
          | c == '.' = [1, 6 + 11]
          | c == '!' = [1, 6 + 12]
          | c == '?' = [1, 6 + 13]
          | c == '\'' = [1, 6 + 14]
          | c == ':' = [1, 6 + 15]
          | c == '-' = [1, 6 + 16]
          | c == ',' = [1, 6 + 17]
          | c == 'Á' = val 'A'
          | c == 'É' = val 'E'
          | c == 'Í' = val 'I'
          | c == 'Ó' = val 'O'
          | c == 'Ú' = val 'U'
          | c == 'Ö' = val 'O'
          | c == 'Ő' = val 'O'
          | c == 'Ü' = val 'U'
          | c == 'Ű' = val 'U'
          -- | otherwise = error $ show c
          | otherwise = [] -- TODO

    pack :: Word8 -> Word8 -> Word8 -> Bool -> Word16
    pack x y z end =
      fromIntegral x `shiftL` 10 .|.
      fromIntegral y `shiftL` 5 .|.
      fromIntegral  z `shiftL` 0 .|.
      if end then 0x8000 else 0x0000

    cons :: (Word8, Word8, Word8) -> [Word8] -> BL.ByteString
    cons (x, y, z) s = BL.pack [lo, hi] <> go s
      where
        w = pack x y z False
        lo = fromIntegral w
        hi = fromIntegral (w `shiftR` 8)


    end :: (Word8, Word8, Word8) -> BL.ByteString
    end (x, y, z) = BL.pack [lo, hi]
      where
        w = pack x y z True
        lo = fromIntegral w
        hi = fromIntegral (w `shiftR` 8)

options :: Parser Options
options = do
    inputPath <- strOption $ mconcat
        [ long "input"
        , short 'i'
        , metavar "FILENAME"
        , help "Memory dump"
        ]
    outputPath <- strOption $ mconcat
        [ long "output"
        , short 'o'
        , metavar "DIR"
        , help "Output directory"
        , value "."
        ]
    pure Options{..}

optionsInfo = info (options <**> helper) $ mconcat
    [ fullDesc
    , header "RatBC disassembler"
    ]
