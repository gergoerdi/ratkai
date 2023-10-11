{-# LANGUAGE LambdaCase, RecordWildCards #-}
module RatBC.Game.ToHL where

import RatBC.Utils
import RatBC.Syntax
import RatBC.Words
import RatBC.Text
import RatBC.Game

import Control.Monad.Identity
import Data.Functor.Const
import Data.Array (Array, elems)
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Bits
import Data.Char
import System.FilePath
import Text.Printf

assemble :: Game Identity -> Game (Const BL.ByteString)
assemble Game{..} = Game
    { msgs1 = Const . mconcat . map zscii . elems . runIdentity $ msgs1
    , msgs2 = Const . mconcat . map zscii {-(mconcat . map (BL.singleton . fromIntegral . ord)) -} . elems . runIdentity $ msgs2
    , dict = Const . (<> BL.singleton 0xff) . foldMap putDictEntry . M.toList . runIdentity $ dict
    , enterRoom = Const . roomwise (putStmts . ensureRet) . runIdentity $ enterRoom
    , afterTurn = Const . putStmts . ensureRet . runIdentity $ afterTurn
    , interactiveGlobal = Const . putInteractive . runIdentity $ interactiveGlobal
    , interactiveLocal = Const . roomwise putInteractive . runIdentity $ interactiveLocal
    , resetState = Const . runIdentity $ resetState
    , helpMap = Const . BL.pack . elems . runIdentity $ helpMap
    }
  where
    putStmts = runPut . mapM_ put

    putInteractive :: [InputDispatch [Stmt]] -> BL.ByteString
    putInteractive = runPut . mapM_ (\(InputDispatch input action) -> mapM_ putWord8 input *> putWord8 0x00 *> mapM_ put action)

    putDictEntry (k, ws) = mconcat
        [ zscii (map toUpper w) <> BL.singleton k
        | w <- ws
        ]

    roomwise :: (a -> BL.ByteString) -> Array Word8 a -> BL.ByteString
    roomwise f = mconcat . map withLength . elems . fmap f

    withLength :: BL.ByteString -> BL.ByteString
    withLength bs | n > 255 = error $ printf "withLength: %d" n
                  | otherwise = BL.singleton (fromIntegral n) <> bs
      where
        n = BL.length bs + 1

ensureRet :: [Stmt] -> [Stmt]
ensureRet stmts = case reverse stmts of
    Ret:_ -> stmts
    MoveTo{}:_ -> stmts
    stmts' -> reverse (Ret : stmts')

writeHLFiles :: FilePath -> Game Identity -> IO ()
writeHLFiles outputPath game = do
    write "dict" $ dict game'
    write "text1" $ msgs1 game'
    write "text2" $ msgs2 game'
    write "enter" $ enterRoom game'
    write "after" $ afterTurn game'
    write "interactive-global" $ interactiveGlobal game'
    write "interactive-local" $ interactiveLocal game'
    write "reset" $ resetState game'
    write "help" $ helpMap game'
  where
    game' = assemble game
    write fileName = BL.writeFile (outputPath </> fileName <.> "bin") . getConst

zscii :: String -> BL.ByteString
zscii = go . concatMap (val . toUpper)
  where
    go = \case
        (x:y:z:s@(_:_)) -> cons (x, y, z) s
        [x, y, z] -> end (x, y, z)
        [x, y] -> end (x, y, 0)
        [x] -> end (x, 0, 0)
        -- [] -> error "zscii: empty string"
        [] -> end (0, 0, 0)

    val :: Char -> [Word8]
    val c | 'A' <= c, c <= 'Z' = [fromIntegral $ 6 + ord c - ord 'A']
          | '0' <= c, c <= '9' = [1, fromIntegral $ 22 + ord c - ord '0']
          | c == ' ' = [2]
          | c == '.' = [3]
          | c == ',' = [4]
          | c == '\n' = [5]
          | c == '?' = [1, 1]
          | c == '\'' = [1, 2]
          | c == ':' = [1, 3]
          | c == '-' = [1, 4]
          | c == '&' = [1, 5]
          | c == '!' = [1, 6]
          | c == 'Á' = val 'A'
          | c == 'É' = val 'E'
          | c == 'Í' = val 'I'
          | c == 'Ó' = val 'O'
          | c == 'Ú' = val 'U'
          | c == 'Ö' = val 'O'
          | c == 'Ő' = val 'O'
          | c == 'Ü' = val 'U'
          | c == 'Ű' = val 'U'
          | otherwise = error $ show c
          -- | otherwise = [] -- TODO

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
