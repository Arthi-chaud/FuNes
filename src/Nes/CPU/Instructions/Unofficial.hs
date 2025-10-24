{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

-- | Unofficial instructions that are combinations of official ones
module Nes.CPU.Instructions.Unofficial (lax, sax, dcp, rra, ahx, shx, shy, shs, lxa, axs) where

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
shx = sh X Y

shy :: AddressingMode -> CPU r ()
shy = sh Y X

shs :: AddressingMode -> CPU r ()
shs = sh S X

{-# INLINE sh #-}
sh :: Register -> Register -> AddressingMode -> CPU r ()
sh reg reg' mode = do
    pc <- getPC
    addr <- getOperandAddr mode
    mask <- withCPUState $ getRegister reg
    off <- withCPUState $ getRegister reg'
    let value = unsafeAddrToByte ((byteToAddr mask .&. (((shiftR addr 8) + 1))) .&. 0xff)
    tmp <- readAddr (pc + 1) ()
    when ((byteToAddr off) + tmp <= 0xff) $
        writeByte value addr ()

ahx :: AddressingMode -> CPU r ()
ahx mode = do
    a <- withCPUState $ getRegister A
    x <- withCPUState $ getRegister X
    y <- withCPUState $ getRegister Y
    eff <- ((byteToAddr y) +) <$> getOperandAddr mode
    let high = unsafeAddrToByte (eff `shiftR` 8)
        mask = high + 1
        value = (a .&. x) .&. mask
    writeByte value eff ()

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
