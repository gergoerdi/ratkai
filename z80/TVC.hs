{-# LANGUAGE BlockArguments #-}
module TVC where

import Z80
import Z80.Utils
import Data.Word
import Data.Bits

videoStart :: Word16
videoStart = 0x8000

syscall :: Word8 -> Z80ASM
syscall op = do
    rst 0x30
    db [op]

printCharC :: Z80ASM
printCharC = syscall 0x01

crtcReg :: Word8
crtcReg = 0x70

crtcData :: Word8
crtcData = 0x71

crtcOut :: Word8 -> Word8 -> Z80ASM
crtcOut reg val = do
    ld A reg
    out [crtcReg] A
    ld A val
    out [crtcData] A

setInterruptHandler :: Location -> Z80ASM
setInterruptHandler addr = do
    ld HL 0x0038
    ld [HL] 0xc3 -- JP
    inc HL
    ld [HL] lo
    inc HL
    ld [HL] hi
  where
    (lo, hi) = wordBytes addr

interleave :: Word8 -> Word8 -> Word8
interleave x y = (x' `shiftL` 1) .|. y'
  where
    x' = spread x
    y' = spread y

-- https://graphics.stanford.edu/~seander/bithacks.html#InterleaveBMN
spread :: Word8 -> Word8
spread b = foldr (\(s, m) b -> (b .|. (b `shiftL` s)) .&. m) b (zip [1, 2, 4] [0x55, 0x33, 0x0f])
