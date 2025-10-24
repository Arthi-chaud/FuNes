{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

-- | Unofficial instructions that are combinations of official ones
module Nes.CPU.Instructions.Unofficial (lax, sax, dcp, rra, ahx, shx, shy, lxa, axs) where

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
shx mode = do
    addr <- getOperandAddr mode
    x <- withCPUState $ getRegister X
    y <- withCPUState $ getRegister Y
    let value = (x .&. unsafeAddrToByte (((shiftR addr 8) + 1)))
        tmp = (addr - (byteToAddr y)) .&. 0xff
    if ((byteToAddr y) + tmp <= 0xff)
        then writeByte value addr ()
        else do
            res <- readByte addr ()
            writeByte res addr ()

shy :: AddressingMode -> CPU r ()
shy = sh Y

ahx :: AddressingMode -> CPU r ()
ahx mode = do
    a <- withCPUState $ getRegister A
    mask <- withCPUState $ getRegister X
    addr <- getOperandAddr mode
    let byte' = a .&. mask .&. ((unsafeAddrToByte (shiftR addr 8)))
    writeByte byte' addr ()

{-# INLINE sh #-}
sh :: Register -> AddressingMode -> CPU r ()
sh reg mode = do
    mask <- withCPUState $ getRegister reg
    addr <- getOperandAddr mode
    let byte' = mask .&. ((unsafeAddrToByte (shiftR addr 8)))
    writeByte byte' addr ()

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
