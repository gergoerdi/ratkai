{-# LANGUAGE RecordWildCards, RankNTypes #-}
module RatBC.Game where

import RatBC.Utils
import RatBC.Syntax
import RatBC.Words
import RatBC.Text

import Control.Monad.Identity
import Data.Word
import Data.Array
import Data.Foldable
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
    , deathPicture :: Word8
    , charSet :: BL.ByteString
    , sprites :: Array Word8 BL.ByteString
    }

data Bank = Bank1 | Bank2

mapStmts :: (Bank -> [Stmt] -> [Stmt]) -> Game Identity -> Game Identity
mapStmts f game@Game{..} = game
    { enterRoom = fmap (fmap (f Bank2)) enterRoom
    , afterTurn = fmap (f Bank1) afterTurn
    , interactiveGlobal = fmap (fmap (fmap (f Bank1))) interactiveGlobal
    , interactiveLocal = fmap (fmap (fmap (fmap (f Bank1)))) interactiveLocal
    }

traverseStmts_ :: (Applicative m) => (Bank -> [Stmt] -> m ()) -> Game Identity -> m ()
traverseStmts_ f game@Game{..} =
    traverse_ (traverse_ (f Bank2)) enterRoom *>
    traverse_ (f Bank1) afterTurn *>
    traverse_ (traverse_ (traverse_ (f Bank1))) interactiveGlobal *>
    traverse_ (traverse_ (traverse_ (traverse_ (f Bank1)))) interactiveLocal

mapStmt :: (Bank -> Stmt -> Stmt) -> Game Identity -> Game Identity
mapStmt f = mapStmts (\bank -> map (f bank))

mapGameF :: (forall a. f a -> g a) -> Game f -> Game g
mapGameF f game@Game{..} = game
    { msgs1 = f msgs1
    , msgs2 = f msgs2
    , dict = f dict
    , enterRoom = f enterRoom
    , afterTurn = f afterTurn
    , interactiveGlobal = f interactiveGlobal
    , interactiveLocal = f interactiveLocal
    , resetState = f resetState
    , helpMap = f helpMap
    }

reflowMessages :: Bool -> Int -> Game Identity -> Game Identity
reflowMessages forceNewline numCols game@Game{..} = game
    { msgs1 = fmap (fmap $ wrapWords forceNewline numCols) msgs1
    , msgs2 = fmap (fmap $ wrapWords forceNewline numCols) msgs2
    }
