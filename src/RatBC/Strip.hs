{-# LANGUAGE RecordWildCards, BlockArguments, LambdaCase, ViewPatterns #-}
module RatBC.Strip (stripGame) where

import RatBC.Utils
import RatBC.Syntax
import RatBC.Words
import RatBC.Text
import RatBC.Pretty
import RatBC.Game
import RatBC.Game.Text

import Control.Arrow ((>>>))
import Control.Monad.State
import Control.Monad.Identity
import Data.Array (Array, array, listArray, elems, assocs, bounds, (!), indices)
import Data.Ix (Ix(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub, sort, (\\))
import Data.Maybe
import Data.Foldable (toList)

stripGame :: Game Identity -> Game Identity
stripGame =
    stripRooms >>>
    stripMessages >>>
    stripWords >>>
    stripInteractive

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

usedMessages :: Game Identity -> ([Val], [Val])
usedMessages Game{..} = both (nub . sort) $ mconcat
    [ bank2 $ elems . runIdentity $ helpMap
    , bank2 $ foldMap stmtsMessages . runIdentity $ enterRoom
    , bank1 $ stmtsMessages . runIdentity $ afterTurn
    , bank1 $ foldMap (foldMap stmtsMessages) . runIdentity $ interactiveGlobal
    , bank1 $ foldMap (foldMap (foldMap stmtsMessages)) . runIdentity $ interactiveLocal
    , bank1 [1..17] -- Builtins used by the interpreter
    , bank2 [minItem..maxItem-1]
    ]
  where
    bank1 vals = (vals, mempty)
    bank2 vals = (mempty, vals)

    stmtsMessages = foldMap stmtMessages
    stmtMessages = \case
        Message msg -> [msg]
        CompactMessage msg -> [msg]
        Assert00 _ msg -> [msg]
        AssertFF _ msg -> [msg]
        AssertHere _ msg -> [msg]
        When00 _ body -> stmtsMessages body
        WhenFF _ body -> stmtsMessages body
        _ -> []

usedWords :: Game Identity -> [Val]
usedWords Game{..} = nub . sort $ mconcat
    [ foldMap wordsOf . runIdentity $ interactiveGlobal
    , foldMap (foldMap wordsOf) . runIdentity $ interactiveLocal
    , [0x00..0x16] \\ [0x12, 0x13] , [0x1a..0x1c] -- Builtin commands
    , [0x64] -- ignored grammar connectives
    ]
  where
    wordsOf :: InputDispatch a -> [Val]
    wordsOf (InputDispatch words _) = words

accessibleRooms :: Game Identity -> [Val]
accessibleRooms Game{..} = S.toList $ flip execState mempty $ do
    mapM_ walkStmt $ runIdentity afterTurn
    go startRoom
  where
    walkStmt :: Stmt -> State (S.Set Val) ()
    walkStmt = \case
        MoveTo room' -> go room'
        _ -> pure ()

    go :: Val -> State (S.Set Val) ()
    go room = do
        done <- gets (room `S.member`)
        unless done do
            modify $ S.insert room
            let stmts = mconcat
                  [ runIdentity enterRoom ! fromIntegral room
                  , mconcat . foldMap toList $ runIdentity interactiveGlobal
                  , mconcat . foldMap toList $ runIdentity interactiveLocal ! fromIntegral room
                  ]
            mapM_ walkStmt stmts

stripMessages :: Game Identity -> Game Identity
stripMessages game@Game{..} = mapStmt remap $ game
    { msgs1 = fmap (compactArray gc1) msgs1
    , msgs2 = fmap (onlyKeep bank2) msgs2
    }
  where
    (bank1, bank2) = usedMessages game

    gc1@(_, remap1) = gcArray (bounds $ runIdentity msgs1) bank1

    remap Bank2 = id
    remap Bank1 = \case
        Message msg -> Message $ f msg
        CompactMessage msg -> CompactMessage $ f msg
        Assert00 var msg -> Assert00 var $ f msg
        AssertFF var msg -> AssertFF var $ f msg
        AssertHere var msg -> AssertHere var $ f msg
        When00 var body -> When00 var $ remap Bank1 <$> body
        WhenFF var body -> WhenFF var $ remap Bank1 <$> body
        stmt -> stmt
      where
        f msg = fromMaybe (error "Message GC screwed up") $ remap1 msg

    onlyKeep bank msgs = listArray (bounds msgs)
      [ if i `elem` bank then trimRight s else mempty | (i, s) <- assocs msgs ]

reflowMessages :: Game Identity -> Game Identity
reflowMessages game@Game{..} = game
    { msgs1 = fmap (fmap $ wrapWords 40) msgs1
    , msgs2 = fmap (fmap $ wrapWords 40) msgs2
    }

stripWords :: Game Identity -> Game Identity
stripWords game@Game{..} = game
    { dict = fmap (M.filterWithKey (\k _ -> k `elem` words)) dict
    }
  where
    words = usedWords game

stripRooms :: Game Identity -> Game Identity
stripRooms game@Game{..} = mapStmt remap $ game
    { enterRoom = fmap (compactArray gc) enterRoom
    , interactiveLocal = fmap (compactArray gc) interactiveLocal
    , helpMap = fmap (compactArray gc) helpMap
    , resetState = fmap (BL.map remapReset) resetState
    }
  where
    rooms = accessibleRooms game

    gc@(_, remapRoom) = gcArray (bounds . runIdentity $ enterRoom) rooms

    remap bank = \case
        MoveTo room -> MoveTo $ fromMaybe (error "Room GC screwed up") $ remapRoom room
        When00 var body -> When00 var $ remap bank <$> body
        WhenFF var body -> WhenFF var $ remap bank <$> body
        stmt -> stmt

    remapReset room = fromMaybe room $ remapRoom room

gcArray :: (Ix i, Enum i) => (i, i) -> [i] -> ((i, i), (i -> Maybe i))
gcArray bounds@(from, _) keep = (bounds', flip M.lookup mapping)
  where
    ranks = zip (sort keep) [from..]
    bounds' = (from, snd . last $ ranks)
    mapping = M.fromList ranks

compactArray :: (Ix i) => ((i, i), (i -> Maybe i)) -> Array i a -> Array i a
compactArray (bounds', mapping) xs = array bounds'
    [ (i', xs ! i)
    | i <- indices xs, Just i' <- pure (mapping i)
    ]

stripInteractive :: Game Identity -> Game Identity
stripInteractive game@Game{..} = game
    { interactiveGlobal = fmap (fmap strip) interactiveGlobal
    , interactiveLocal = fmap (fmap (fmap strip)) interactiveLocal
    }
  where
    strip :: InputDispatch a -> InputDispatch a
    strip (InputDispatch words x) = InputDispatch (filter isValidWord words) x

    isValidWord = (`M.member` runIdentity dict)
