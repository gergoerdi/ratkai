{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module RatBC.Pretty where

import RatBC.Syntax

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

type Msgs = Array Val String

encloseVSep :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseVSep l r s ds = case ds of
    []  -> l <> r
    -- [d] -> l <+> d <+> r
    _   -> vcat [vcat (zipWith (<+>) (l : repeat s) (map align ds)), r]

vlist :: [Doc ann] -> Doc ann
vlist = encloseVSep lbracket rbracket comma

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

niceStmt :: Msgs -> Stmt -> Doc ann
niceStmt msgs = \case
    Message msg -> message msg
    Assert00 _ msg -> message msg
    AssertFF _ msg -> message msg
    AssertHere _ msg -> message msg
    _ -> mempty
  where
    message msg = "--" <+> fromString (msgs ! msg)

pprInputDispatch :: M.Map Word8 [String] -> Msgs -> InputDispatch -> Doc ann
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
