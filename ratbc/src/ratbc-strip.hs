{-# LANGUAGE ApplicativeDo, RecordWildCards, TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RatBC.Utils
import RatBC.Syntax
import RatBC.Words
import RatBC.Text
import RatBC.Pretty
import RatBC.Game
import RatBC.Game.Text
import RatBC.Game.ToHL

import Control.Monad.State
import Data.Functor.Const
import Control.Monad.Identity
import Data.Array (Array, listArray, elems, assocs, bounds, (!))
import Prettyprinter
import Prettyprinter.Render.String
import Data.String
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Set as S
import Options.Applicative
import Control.Monad
import Data.List.Split
import Data.List (nub, sort)
import Text.Printf
import Data.Word
import Data.Maybe
import Data.Either
import Data.Char
import System.Directory
import System.FilePath
import Data.Foldable (toList)

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
    Sleep n -> Just $ Sleep $ min 10 n
    CopyProtection{} -> Nothing
    MachineCode addr _ -> Just $ MachineCode addr [0x60]
    -- If00 var body -> If00 var <$> let body' = mapMaybe stripHomeLab body in body' <$ guard (not . null $ body')
    -- IfFF var body -> IfFF var <$> let body' = mapMaybe stripHomeLab body in body' <$ guard (not . null $ body')
    s -> pure s

transformStmts :: ([Stmt] -> [Stmt]) -> (Game Identity -> Game Identity)
transformStmts f game = game
    { enterRoom = fmap (fmap f) $ enterRoom game
    , afterTurn = fmap f $ afterTurn game
    , interactiveGlobal = fmap (fmap (fmap f)) $ interactiveGlobal game
    , interactiveLocal = fmap (fmap (fmap (fmap f))) $ interactiveLocal game
    }

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

usedMessages :: Game Identity -> ([Val], [Val])
usedMessages Game{..} = both (nub . sort) $ mconcat
    [ bank2 $ elems . runIdentity $ helpMap
    , bank2 $ foldMap stmtsMessages . runIdentity $ enterRoom
    , bank1 $ stmtsMessages . runIdentity $ afterTurn
    , bank1 $ foldMap (foldMap stmtsMessages) . runIdentity $ interactiveGlobal
    , bank1 $ foldMap (foldMap (foldMap stmtsMessages)) . runIdentity $ interactiveLocal
    , bank1 [1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 16, 17] -- Builtins used by the interpreter
    , bank2 [minItem..maxItem]
    ]
  where
    bank1 vals = (vals, mempty)
    bank2 vals = (mempty, vals)

    stmtsMessages = foldMap stmtMessages
    stmtMessages = \case
        Message msg -> [msg]
        Assert00 _ msg -> [msg]
        AssertFF _ msg -> [msg]
        AssertHere _ msg -> [msg]
        _ -> []

usedWords :: Game Identity -> [Val]
usedWords Game{..} = nub . sort $ mconcat
    [ foldMap wordsOf . runIdentity $ interactiveGlobal
    , foldMap (foldMap wordsOf) . runIdentity $ interactiveLocal
    , [0x00..0x1c] -- Builtin commands
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

stripMessages :: [Val] -> [Val] -> Game Identity -> Game Identity
stripMessages bank1 bank2 game@Game{..} = game
    { msgs1 = fmap (onlyKeep bank1) msgs1
    , msgs2 = fmap (onlyKeep bank2) msgs2
    }
  where
    onlyKeep bank msgs = listArray (bounds msgs)
      [ if i `elem` bank then trimRight s else mempty | (i, s) <- assocs msgs ]

trimRight :: String -> String
trimRight = reverse . dropWhile (== ' ') . reverse

stripWords :: [Val] -> Game Identity -> Game Identity
stripWords words game@Game{..} = game
    { dict = fmap (M.filterWithKey (\k _ -> k `elem` words)) dict
    }

stripRooms :: [Val] -> Game Identity -> Game Identity
stripRooms rooms game@Game{..} = game
    { enterRoom = fmap strip enterRoom
    , interactiveLocal = fmap strip interactiveLocal
    }
  where
    strip :: (Monoid a) => ByRoom a -> ByRoom a
    strip xs = listArray (bounds xs)
      [ if i `elem` rooms then x else mempty | (i, x) <- assocs xs ]

stripInteractive :: Game Identity -> Game Identity
stripInteractive game@Game{..} = game
    { interactiveGlobal = fmap (fmap strip) interactiveGlobal
    , interactiveLocal = fmap (fmap (fmap strip)) interactiveLocal
    }
  where
    strip :: InputDispatch a -> InputDispatch a
    strip (InputDispatch words x) = InputDispatch (filter isValidWord words) x

    isValidWord = (`M.member` runIdentity dict)

main :: IO ()
main = do
    opts@Options{..} <- execParser optionsInfo

    game <- loadTextFiles inputPath

    let f = mapMaybe stripHomeLab

    game <- pure $ transformStmts f game
    let rooms = accessibleRooms game
    print rooms
    game <- pure $ stripRooms rooms game

    let (bank1, bank2) = usedMessages game
        words = usedWords game
    print $ nub . sort $ bank1
    print $ nub . sort $ bank2

    game <- pure $ stripMessages bank1 bank2 game
    game <- pure $ stripWords words game
    game <- pure $ stripInteractive game

    createDirectoryIfMissing True outputPath
    writeTextFiles outputPath game
    writeHLFiles outputPath game

options :: Parser Options
options = do
    inputPath <- strOption $ mconcat
        [ long "input"
        , short 'i'
        , metavar "DIR"
        , help "Input directory"
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
