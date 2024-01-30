{-# LANGUAGE LambdaCase, RecordWildCards, TypeApplications #-}
module RatBC.HomeLab34.Binary (fixupSkips, assemble, writeFiles) where

import RatBC.Utils
import RatBC.Syntax
import RatBC.Words
import RatBC.Text
import RatBC.Game

import RatBC.HomeLab34.Text

import Control.Monad.Identity
import Control.Monad.RevState hiding (put)
import qualified Control.Monad.RevState as RSt
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
import Data.Foldable (traverse_)

fixupSkips :: Game Identity -> Game Identity
fixupSkips game@Game{..} = game
    { enterRoom = fmap (fmap assertNoSkips) enterRoom
    , afterTurn = fmap assertNoSkips afterTurn
    , interactiveGlobal = fmap fixupGlobal interactiveGlobal
    , interactiveLocal = fmap fixupLocal interactiveLocal
    }
  where
    fixupGlobal = flip evalState (error "invalid skip in global") . reskipRoom
    fixupLocal = flip evalState (error "invalid skip in local") . traverse reskipRoom

    reskip :: InputDispatch [Stmt] -> State Word8 (InputDispatch [Stmt])
    reskip (InputDispatch input action) = InputDispatch input <$> do
            RSt.put $ fromIntegral (length input) + 3
            action' <- case reverse action of
                Skip{}:action' -> do
                    skipToNext <- RSt.get
                    pure $ reverse $ Skip skipToNext : map assertNoSkip action'
                _ -> pure $ map assertNoSkip action
            pure action'

    reskipRoom :: [InputDispatch [Stmt]] -> State Word8 [InputDispatch [Stmt]]
    reskipRoom room = do
        room' <- traverse reskip room
        modify (+ 2)
        pure room'

assemble :: Game Identity -> Game (Const BL.ByteString)
assemble game@Game{..} = game
    { msgs1 = Const . mconcat . msgs . elems . runIdentity $ msgs1
    , msgs2 = Const . mconcat . msgs . elems . runIdentity $ msgs2
    , dict = Const . (<> BL.singleton 0xff) . foldMap putDictEntry . M.toList . runIdentity $ dict
    , enterRoom = Const . runPut . roomwise putStmts . runIdentity $ enterRoom
    , afterTurn = Const . runPut . putStmts . runIdentity $ afterTurn
    , interactiveGlobal = Const . runPut . putInteractive . runIdentity $ interactiveGlobal
    , interactiveLocal = Const . runPut . roomwise putInteractive . runIdentity $ interactiveLocal
    , resetState = Const . runIdentity $ resetState
    , helpMap = Const . BL.pack . elems . runIdentity $ helpMap
    }
  where
    putStmts = mapM_ put . ensureRet

    putInteractive :: [InputDispatch [Stmt]] -> Put
    putInteractive responses = do
        traverse_ (withLength8 . putDispatch) responses
        putWord8 0x00
      where
        putDispatch (InputDispatch input action) = do
            mapM_ putWord8 input
            putWord8 0x00
            putStmts action

    text = runPut . mapM_ (putWord8 . encodeChar)
    msgs = map (runPut . withLength8 . putLazyByteString . text)

    putDictEntry (k, ws) = mconcat
        [ text (map toUpper w) <> BL.singleton k
        | w <- ws
        ]

    roomwise :: (a -> Put) -> Array Word8 a -> Put
    roomwise f = traverse_ (withLength8 . f)

    withLength8 :: Put -> Put
    withLength8 body
        | n > fromIntegral (maxBound @Word8) = error $ printf "withLength8: %d" n
        | otherwise = putWord8 (fromIntegral n) *> putLazyByteString bs
      where
        bs = runPut body
        n = BL.length bs + 1

ensureRet :: [Stmt] -> [Stmt]
ensureRet stmts = case reverse stmts of
    Ret:_ -> stmts
    Skip{}:_ -> stmts
    stmts' -> reverse (Ret : stmts')

assertNoSkip :: Stmt -> Stmt
assertNoSkip = \case
    -- Skip{} -> error "Skip"
    When00 var body -> When00 var $ assertNoSkips body
    WhenFF var body -> WhenFF var $ assertNoSkips body
    stmt -> stmt

assertNoSkips :: [Stmt] -> [Stmt]
assertNoSkips = map assertNoSkip

writeFiles :: FilePath -> Game Identity -> IO ()
writeFiles outputPath game = do
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
