{-# LANGUAGE RecordWildCards, BlockArguments, LambdaCase, ViewPatterns #-}
module RatBC.TVC.Strip (preprocessGame) where

import RatBC.Syntax
import RatBC.Game
import RatBC.Strip
import RatBC.TVC.Binary (fixupSkips)
import RatBC.TVC.Picture

import Control.Monad.Identity
import Data.Maybe
import Text.Printf

preprocessGame :: Game Identity -> Game Identity
preprocessGame = fixupSkips . stripGame . stripScript

stripScript :: Game Identity -> Game Identity
stripScript = mapStmts \_ -> mapMaybe stripStmt

stripStmt :: Stmt -> Maybe Stmt
stripStmt = \case
    SetScreen border bg pic -> Just $ SetScreen (toBorderColor border) (toBackgroundColor bg) pic
    SetTextColors output input -> Just $ SetTextColors (toDenseColor output) (toDenseColor input)
    SpriteOn i addr col x y -> Just $ SpriteOn i addr (toBorderColor col) x' y' -- TODO: transform x and y
      where
        x' = fromInteger $ max 0 $ (fromIntegral x - 106) `div` 4
        y' = fromInteger $ max 0 $ fromIntegral y - 58
    Chime{} -> Nothing
    Sleep n -> Just $ Sleep $ min 30 n
    CopyProtection{} -> Nothing
    MachineCode addr ops
      | [0xa9, 0x5b, 0xa0, 0x99, 0x99, 0x00, 0x20, 0x60] <- ops -> Just $ Assign 0x99 0x5b
      | [0xa9, 0x4d, 0xa0, 0x94, 0x99, 0x00, 0x20, 0x60] <- ops -> Just $ Assign 0x94 0x4d
      | [0xa0, 0x78, 0xb9, 0x00, 0x20, 0xd0, 0x05, 0xa9, 0x5f, 0x99, 0x00, 0x20, 0xc8, 0xc0, 0xa0, 0xd0, 0xf1, 0x60] <- ops -> Just $ MachineCode addr []
      | otherwise -> error $ printf "MachineCode: %d %s" (length ops) (show ops)
    When00 var body -> When00 var <$> let body' = mapMaybe stripStmt body in body' <$ guard (not . null $ body')
    WhenFF var body -> WhenFF var <$> let body' = mapMaybe stripStmt body in body' <$ guard (not . null $ body')
    Message msg -> Just $ CompactMessage msg
    s -> pure s
