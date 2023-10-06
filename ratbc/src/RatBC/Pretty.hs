{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module RatBC.Pretty where

import RatBC.Syntax

import Prettyprinter
import Data.String
import Data.Array
import Data.Binary (put)
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Text.Printf

type Msgs = Array Val String

pprStmts :: Msgs -> [Stmt] -> Doc ann
pprStmts msgs stmts = braces $ line <> indent 2 (vcat $ map (pprStmt msgs) stmts) <> line

pprStmt :: Msgs -> Stmt -> Doc ann
pprStmt msgs stmt = hsep
  [ fill 30 $ fromString $ show stmt
  , "--"
  , fill 20 $ pprBytes $ runPut $ put stmt
  , niceStmt msgs stmt
  ]

pprBytes :: BL.ByteString -> Doc ann
pprBytes bs = hsep [ fromString $ printf "%02x" b | b <- BL.unpack bs ]

niceStmt :: Msgs -> Stmt -> Doc ann
niceStmt msgs = \case
    Message msg -> message msg
    Assert00 _ msg -> message msg
    AssertFF _ msg -> message msg
    AssertHere _ msg -> message msg
    _ -> mempty
  where
    message msg = "--" <+> fromString (msgs ! msg)
