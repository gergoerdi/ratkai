{-# LANGUAGE RecordWildCards, BlockArguments, LambdaCase, ViewPatterns #-}
module RatBC.TVC.Strip (preprocessGame) where

import RatBC.Syntax
import RatBC.Game
import RatBC.Strip
import RatBC.TVC.Binary (fixupSkips)
import RatBC.TVC.Picture

import Control.Monad.Identity
import Data.Maybe

preprocessGame :: Game Identity -> Game Identity
preprocessGame = fixupSkips . stripGame . stripScript

stripScript :: Game Identity -> Game Identity
stripScript = mapStmts \_ -> mapMaybe stripStmt

stripStmt :: Stmt -> Maybe Stmt
stripStmt = \case
    SetScreen border bg pic -> Just $ SetScreen (toBorderColor border) (toBackgroundColor bg) pic
    SetTextColors output input -> Just $ SetTextColors (toBackgroundColor output) (toBackgroundColor input)
    SpriteOn i addr col x y -> Just $ SpriteOn i addr (toBorderColor col) x' y' -- TODO: transform x and y
      where
        x' = max 0 $ (x - 106) `div` 4
        y' = y - 58
    Chime{} -> Nothing
    Sleep n -> Just $ Sleep $ min 10 n
    CopyProtection{} -> Nothing
    MachineCode addr _ -> Nothing -- TODO
    When00 var body -> When00 var <$> let body' = mapMaybe stripStmt body in body' <$ guard (not . null $ body')
    WhenFF var body -> WhenFF var <$> let body' = mapMaybe stripStmt body in body' <$ guard (not . null $ body')
    Message msg -> Just $ CompactMessage msg
    s -> pure s
