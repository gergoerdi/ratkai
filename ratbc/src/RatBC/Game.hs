{-# LANGUAGE RecordWildCards #-}
module RatBC.Game where

import RatBC.Utils
import RatBC.Syntax
import RatBC.Words
import RatBC.Text

import Control.Monad.Identity
import Data.Word
import Data.Array
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M

type Msgs = Array Val String
type Dict = M.Map Word8 [String]
type ByRoom a = Array Word8 a

data Game f = Game
    { msgs1, msgs2 :: f Msgs
    , dict :: f Dict
    , enterRoom :: f (ByRoom [Stmt])
    , afterTurn :: f [Stmt]
    , interactiveGlobal :: f [InputDispatch [Stmt]]
    , interactiveLocal :: f (ByRoom [InputDispatch [Stmt]])
    , resetState :: f BL.ByteString
    , helpMap :: f (ByRoom Word8)
    , minItem, maxItem :: Word8
    , startRoom :: Word8
    }

mapStmts :: ([Stmt] -> [Stmt]) -> Game Identity -> Game Identity
mapStmts f game@Game{..} = game
  { enterRoom = fmap (fmap f) enterRoom
  , afterTurn = fmap f afterTurn
  , interactiveGlobal = fmap (fmap (fmap f)) interactiveGlobal
  , interactiveLocal = fmap (fmap (fmap (fmap f))) interactiveLocal
  }
