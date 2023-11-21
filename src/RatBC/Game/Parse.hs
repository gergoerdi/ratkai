{-# LANGUAGE ApplicativeDo, RecordWildCards, TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module RatBC.Game.Parse where

import RatBC.Utils
import RatBC.Syntax
import RatBC.Words
import RatBC.Text
import RatBC.Game
import RatBC.Pretty

import Text.Trifecta
-- import Prettyprinter (renderPretty, line')

import Data.Tuple.Solo
import Data.Foldable (asum)
import Control.Monad.Identity
import Data.Functor.Const
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Word
import Data.Array (Array, array, listArray)
import Text.Printf

parseGame :: Game (Const String) -> Game Identity
parseGame game@Game{..} = game
    { msgs1 = Identity . parseMessages "text1" . getConst $ msgs1
    , msgs2 = Identity . parseMessages "text2" . getConst $ msgs2
    , dict = Identity . parseWords . getConst $ dict
    , enterRoom = Identity . fromList . parseFrom "enter" . getConst $ enterRoom
    , afterTurn = Identity . parseFrom "after" . getConst $ afterTurn
    , interactiveGlobal = Identity . parseFrom "interactive-global" . getConst $ interactiveGlobal
    , interactiveLocal =  Identity . fromList . parseFrom "interactive-local" . getConst $ interactiveLocal
    , resetState = Identity . BL.pack . parseFrom "reset" . getConst $ resetState
    , helpMap = Identity . fromList . parseFrom "help" . getConst $ helpMap
    }
  where
    fromList :: [a] -> Array Word8 a
    fromList xs = listArray (1, fromIntegral $ length xs) xs

    parseFrom :: (Parse a, Show a) => String -> String -> a
    parseFrom tag s = case parseString (spaces *> parse <* eof) mempty s of
        Success x -> x
        Failure ErrInfo{..} ->
          error $ printf "Parse error in '%s': %s" tag (show _errDoc)

    parseMessages :: String -> String -> Array Word8 String
    parseMessages tag s = let xs = parseFrom tag s
      in array (1, fromIntegral $ length xs) [(i, x) | (i, Solo x) <- xs]

    parseWords = fmap (map getSolo) . M.fromList . parseFrom "dict"

class Parse a where
    parse :: Parser a

instance Parse Stmt where
    parse = asum
        [ p0 "Ret" Ret
        , p2 "Assign" Assign
        , p1 "Message" Message
        , p1 "Assign00" Assign00
        , p1 "AssignFF" AssignFF
        , p1 "AssignLoc" AssignLoc
        , p2 "Assert00" Assert00
        , p2 "AssertFF" AssertFF
        , p2 "AssertHere" AssertHere
        , p1 "Skip" Skip
        , p2 "If00" If00
        , p2 "IfFF" IfFF
        , p1 "MoveTo" MoveTo
        , p1 "SetPlayerStatus" SetPlayerStatus
        , p1 "Heal" Heal
        , p1 "Hurt" Hurt
        , p1 "AddScore" AddScore
        , p3 "SetScreen" SetScreen
        , p5 "SpriteOn" SpriteOn
        , p1 "SpriteOff" SpriteOff
        , p1 "Chime" Chime
        , p1 "Sleep" Sleep
        , p1 "IncIfNot0" IncIfNot0
        , p2 "MachineCode" MachineCode
        , p4 "CopyProtection" CopyProtection
        , p2 "When00" When00
        , p2 "WhenFF" WhenFF
        , p1 "CompactMessage" CompactMessage
        ]
      where
        p0 s x = try $ keyword s *> pure x
        p1 s f = try $ keyword s *> (f <$> parse)
        p2 s f = try $ keyword s *> (f <$> parse <*> parse)
        p3 s f = try $ keyword s *> (f <$> parse <*> parse <*> parse)
        p4 s f = try $ keyword s *> (f <$> parse <*> parse <*> parse <*> parse)
        p5 s f = try $ keyword s *> (f <$> parse <*> parse <*> parse <*> parse <*> parse)

instance Parse (Solo String) where
    parse = Solo <$> stringLiteral

instance Parse a => Parse [a] where
    parse = brackets $ commaSep parse

instance (Parse a, Parse b) => Parse (a, b) where
    parse = parens $ (,) <$> parse <* comma <*> parse

instance Parse Word8 where
    parse = fromIntegral <$> natural

instance Parse Word16 where
    parse = fromIntegral <$> natural

instance Parse a => Parse (InputDispatch a) where
    parse = keyword "InputDispatch" *> (InputDispatch <$> parse <*> parse)

keyword :: String -> Parser String
keyword s = token $ string s <* notFollowedBy alphaNum
