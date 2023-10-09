{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards, NamedFieldPuns #-}
module RatBC.Pretty where

import RatBC.Syntax
import RatBC.Game as Game

import Control.Monad.Identity
import Data.Functor.Const

import Prettyprinter
import Data.String
import Data.Array
import Data.Binary (put)
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Text.Printf
import Data.Word
import Data.Maybe

encloseVSep :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseVSep l r s ds = case ds of
    []  -> l <> r
    -- [d] -> l <+> d <+> r
    _   -> vcat [vcat (zipWith (<+>) (l : repeat s) (map align ds)), r]

vlist :: [Doc ann] -> Doc ann
vlist = encloseVSep lbracket rbracket comma

tuple :: [Doc ann] -> Doc ann
tuple = encloseSep lparen rparen comma

pprStmts :: Msgs -> [Stmt] -> Doc ann
pprStmts msgs = vlist . map (pprStmt msgs)

pprStmt :: Msgs -> Stmt -> Doc ann
pprStmt msgs stmt = hsep
  [ fill 30 $ viaShow stmt
  , "--"
  , fill 20 $ pprBytes $ runPut $ put stmt
  , niceStmt msgs stmt
  ]

pprBytes :: BL.ByteString -> Doc ann
pprBytes bs = hsep [ fromString $ printf "%02x" b | b <- BL.unpack bs ]

pprMessage :: Msgs -> Val -> Doc ann
pprMessage msgs msg
  | inRange (bounds msgs) msg = "--" <+> fromString (msgs ! msg)
  | otherwise = mempty

niceStmt :: Msgs -> Stmt -> Doc ann
niceStmt msgs = \case
    Message msg -> pprMessage msgs msg
    Assert00 _ msg -> pprMessage msgs msg
    AssertFF _ msg -> pprMessage msgs msg
    AssertHere _ msg -> pprMessage msgs msg
    _ -> mempty

pprInputDispatch :: Dict -> Msgs -> InputDispatch -> Doc ann
pprInputDispatch dict msgs (InputDispatch input stmts) = vcat
    [ hsep
      [ fill 20 $ "InputDispatch" <+> list (map (fromString . printf "0x%02x") input)
      , "--"
      , pprWords input
      ]
    , indent 2 $ pprStmts msgs stmts
    ]
  where
     pprWords bs = hsep
       [ fromString $ head words
       | b <- bs
       , let words = fromMaybe [printf "|%02x|" b] $ M.lookup b dict
       ]

pprGame :: Game Identity -> Game (Const (Doc ann))
pprGame game@Game{enterRoom, afterTurn, interactiveLocal, interactiveGlobal, helpMap, resetState} = Game
  { msgs1 = Const . pprMessages $ msgs1
  , msgs2 = Const . pprMessages $ msgs2
  , dict = Const . pprDict $ dict
  , enterRoom = Const . perRoom (pprStmts msgs2) . runIdentity $ enterRoom
  , afterTurn = Const $ pprStmts msgs1 . runIdentity $ afterTurn
  , interactiveGlobal = Const $ vlist $ map (pprInputDispatch dict msgs1) . runIdentity $ interactiveGlobal
  , interactiveLocal = Const . perRoom (vlist . map (pprInputDispatch dict msgs1)) . runIdentity $ interactiveLocal
  , helpMap = Const . perRoom (pprHelp msgs2) . runIdentity $ helpMap
  , resetState = Const . viaShow . BL.unpack . runIdentity $ resetState
  }
  where
    msgs1 = runIdentity $ Game.msgs1 game
    msgs2 = runIdentity $ Game.msgs2 game
    dict = runIdentity $ Game.dict game

    pprMessages msgs = vcat [ fill 4 (viaShow i <> colon) <+> fromString s | (i, s) <- assocs msgs ]
    pprDict dict = vlist
      [ tuple [fromString (printf "0x%02x" i), list [dquotes $ fromString word | word <- words]]
      | (i, words) <- M.toList dict
      ]

    pprHelp msgs i = viaShow i <+> pprMessage msgs i

    perRoom :: (a -> Doc ann) -> ByRoom a -> Doc ann
    perRoom f rooms = vlist
      [ vcat [ "--" <+> pprRoom room
             , f x
             ]
      | (room, x) <- assocs rooms
      ]

    pprRoom :: Word8 -> Doc ann
    pprRoom room = "ROOM" <+> viaShow room
