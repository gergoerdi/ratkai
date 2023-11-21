{-# LANGUAGE RecordWildCards, BlockArguments, LambdaCase, ViewPatterns #-}
module RatBC.TVC.Strip (preprocessGame) where

import RatBC.Syntax
import RatBC.Game
import RatBC.Strip
import RatBC.TVC.Binary (fixupSkips)

import Control.Monad.Identity
import Data.Maybe

preprocessGame :: Game Identity -> Game Identity
preprocessGame = fixupSkips . {- stripGame . -} stripScript

stripScript :: Game Identity -> Game Identity
stripScript = transformStmts (mapMaybe stripStmt)

stripStmt :: Stmt -> Maybe Stmt
stripStmt = \case
    Chime{} -> Nothing
    Sleep n -> Just $ Sleep $ min 10 n
    CopyProtection{} -> Nothing
    MachineCode addr _ -> Nothing -- TODO
    When00 var body -> When00 var <$> let body' = mapMaybe stripStmt body in body' <$ guard (not . null $ body')
    WhenFF var body -> WhenFF var <$> let body' = mapMaybe stripStmt body in body' <$ guard (not . null $ body')
    Message msg -> Just $ CompactMessage msg
    s -> pure s

transformStmts :: ([Stmt] -> [Stmt]) -> (Game Identity -> Game Identity)
transformStmts f game = game
    { enterRoom = fmap (fmap f) $ enterRoom game
    , afterTurn = fmap f $ afterTurn game
    , interactiveGlobal = fmap (fmap (fmap f)) $ interactiveGlobal game
    , interactiveLocal = fmap (fmap (fmap (fmap f))) $ interactiveLocal game
    }
