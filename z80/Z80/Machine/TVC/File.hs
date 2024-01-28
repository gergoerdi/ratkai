{-# LANGUAGE BlockArguments, NumericUnderscores, BinaryLiterals, RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
module Z80.Machine.TVC.File where

import Z80
import Z80.Utils
import Z80.Machine.TVC.Defs

import Data.Word
import Control.Monad

-- | Pre: `DE` points to filename preceded by filename length
createFile_ :: Z80ASM
createFile_ = do
    ldVia A [0x0b6b] 0
    syscall 0x53

-- | Pre: `DE` points to data
-- | Pre: `BC` points to number of bytes
-- | Post: `BC` is number of bytes not written
writeBlock_ :: Z80ASM
writeBlock_ = syscall 0x52

closeFile_ :: Z80ASM
closeFile_ = syscall 0x54

-- | Pre: `DE` points to filename preceded by filename length
openFile_ :: Z80ASM
openFile_ = syscall 0xd3

-- | Pre: `DE` points to data
-- | Pre: `BC` points to number of bytes
-- | Post: `BC` is number of bytes not read
readBlock_ :: Z80ASM
readBlock_ = syscall 0xd2
