module Nes.CPU.Instructions.DE (dec, dex, dey) where

import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.After (setZeroAndNegativeFlags)
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory

-- | Decrement value in memory
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#DEC
dec :: AddressingMode -> CPU r ()
dec mode = do
    addr <- getOperandAddr mode
    res <- (+ (-1)) <$> withBus (readByte addr)
    withBus $ writeByte res addr
    setZeroAndNegativeFlags res

-- | Decrement X register
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#DEX
dex :: CPU r ()
dex = do
    res <- (\x -> x - 1) <$> getRegister X
    setRegister X res
    setZeroAndNegativeFlags res

-- | Decrement Y register
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#DEY
dey :: CPU r ()
dey = do
    res <- (\y -> y - 1) <$> getRegister Y
    setRegister Y res
    setZeroAndNegativeFlags res
