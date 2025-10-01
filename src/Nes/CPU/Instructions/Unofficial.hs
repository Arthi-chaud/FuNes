-- | Unofficial instructions that are combinations of official ones
module Nes.CPU.Instructions.Unofficial (lax, sax, dcp, rra) where

import Control.Monad
import Data.Bits
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.After (setZeroAndNegativeFlags)
import Nes.CPU.Instructions.Arith (addToRegisterA)
import Nes.CPU.Instructions.Bitwise (ror_)
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
