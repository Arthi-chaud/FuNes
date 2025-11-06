{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

-- | Unofficial instructions that are combinations of official ones
module Nes.CPU.Instructions.Unofficial (lax, sax, dcp, rra, sha, shx, shy, shs, lxa, axs) where

import Control.Monad
import Data.Bits
import Nes.CPU.Instructions.Access (lda)
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.After (setZeroAndNegativeFlags)
import Nes.CPU.Instructions.Arith (addToRegisterA)
import Nes.CPU.Instructions.Bitwise (ror_)
import Nes.CPU.Instructions.Transfer
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.FlagRegister
import Nes.Memory

-- Source: https://www.nesdev.org/wiki/Programming_with_unofficial_opcodes

-- | Equivalent to LDA then TAX, saves a couple of cycles
lax :: AddressingMode -> CPU r ()
lax mode = do
    addr <- getOperandAddr mode
    byte <- readByte addr ()
    modifyCPUState $
        setRegister A byte
            . setRegister X byte
    setZeroAndNegativeFlags byte

-- | Stores the bitwise AND of A and X. No flags are affected
sax :: AddressingMode -> CPU r ()
sax mode = do
    dest <- getOperandAddr mode
    a <- withCPUState $ getRegister A
    x <- withCPUState $ getRegister X
    let res = a .&. x
    writeByte res dest ()

-- | Equivalent to DEC value then CMP value, except supporting more addressing modes
dcp :: AddressingMode -> CPU r ()
dcp mode = do
    addr <- getOperandAddr mode
    value <- (+ (-1)) <$> readByte addr ()
    writeByte value addr ()
    a <- withCPUState $ getRegister A
    tickOnce -- It's a Read-modify-write operation
    when (value <= a) $ modifyCPUState $ modifyStatusRegister (setFlag Carry)
    setZeroAndNegativeFlags (a - value)

-- | (Unofficial) ROR and ADC
rra :: AddressingMode -> CPU r ()
rra mode = do
    value <- ror_ mode
    addToRegisterA value

-- | Source: https://forums.nesdev.org/viewtopic.php?t=8107
shx :: AddressingMode -> CPU r ()
shx = sh X

shy :: AddressingMode -> CPU r ()
shy = sh Y

{-# INLINE sh #-}
-- https://github.com/100thCoin/AccuracyCoin/blob/9f7de1130d8d590baf7a7add0cc33ccd04137ada/AccuracyCoin.asm#L2866
sh :: Register -> AddressingMode -> CPU r ()
sh reg mode = do
    x <- withCPUState $ getRegister reg
    operand <- getPC >>= flip readAddr ()
    (originalDest, crosses) <- getOperandAddr' mode
    let high = unsafeAddrToByte (operand `shiftR` 8)
        destHigh = (high + 1) .&. x
        value = destHigh
        dest =
            if crosses
                then ((byteToAddr destHigh) `shiftL` 8) .|. (originalDest .&. 0xff)
                else originalDest
    writeByte value dest ()

sha :: AddressingMode -> CPU r ()
sha mode = do
    a <- withCPUState $ getRegister A
    x <- withCPUState $ getRegister X
    y <- withCPUState $ getRegister Y
    (originalDest, crosses) <- getOperandAddr' mode
    let destWithoutY = originalDest - byteToAddr y
        high = unsafeAddrToByte (destWithoutY `shiftR` 8)
        value = (high + 1) .&. a .&. x
        destHigh = (high + 1) .&. a .&. x
        dest =
            if crosses
                then ((byteToAddr destHigh) `shiftL` 8) .|. (originalDest .&. 0xff)
                else originalDest
    writeByte value dest ()

shs :: AddressingMode -> CPU r ()
shs mode = do
    a <- withCPUState $ getRegister A
    x <- withCPUState $ getRegister X
    operand <- getPC >>= flip readAddr ()
    let s = a .&. x
    modifyCPUState $ setRegister S s
    (originalDest, crosses) <- getOperandAddr' mode
    let high = unsafeAddrToByte (operand `shiftR` 8)
        destHigh = (high + 1) .&. a .&. x
        value = (high + 1) .&. s
        dest =
            if crosses
                then ((byteToAddr destHigh) `shiftL` 8) .|. (originalDest .&. 0xff)
                else originalDest
    writeByte value dest ()

lxa :: AddressingMode -> CPU r ()
lxa = lda >=> const tax

axs :: AddressingMode -> CPU r ()
axs mode = do
    addr <- getOperandAddr mode
    byte <- readByte addr ()
    x <- withCPUState $ getRegister X
    a <- withCPUState $ getRegister A
    let xAndA = x .&. a
    let res = xAndA - byte
    modifyCPUState $
        modifyStatusRegister
            (setFlag' Carry (byte <= xAndA))
            . setRegister X res
    setZeroAndNegativeFlags res
