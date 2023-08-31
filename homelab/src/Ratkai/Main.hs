{-# LANGUAGE RecordWildCards, RecursiveDo, BlockArguments #-}
module Ratkai.Main (game) where

import Z80
import Z80.Utils
import HL2

game :: Z80ASM
game = mdo
    loopForever $ pure ()
    pure ()
