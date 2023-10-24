{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module RatBC.Syntax where

import Data.Word
import Data.Binary
import Control.Applicative
import Text.Printf
import Debug.Trace
import qualified Data.ByteString.Lazy as BL

type Addr = Word16
type Var = Word8
type Val = Word8
type Msg = Val

data InputDispatch a = InputDispatch [Word8] a
    deriving (Show, Read, Functor, Foldable)

data Stmt
    = Ret
    | Assign Var Val
    | Message Msg
    | Assign00 Var
    | AssignFF Var
    | AssignLoc Var
    | Assert00 Var Msg
    | AssertFF Var Msg
    | AssertHere Var Msg
    | Skip Val
    | If00 Var Val
    | IfFF Var Val
    | MoveTo Val
    | SetPlayerStatus Val
    | Heal Val
    | Hurt Val
    | AddScore Val
    | SetScreen Val Val Val
    | SpriteOn Val Val Val Val Val
    | SpriteOff Val
    | Chime Val
    | Sleep Val
    | IncIfNot0 Var
    | MachineCode Addr [Word8]
    | CopyProtection Val Val Val Val

    | When00 Var [Stmt]
    | WhenFF Var [Stmt]
    | CompactMessage Msg
    deriving (Show, Read)
