module Nes.CPU.Instructions.ST (sta, stx, sty) where

import Nes.CPU.Instructions.Addressing
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory

-- | Store the value of the A register in memory
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#STA
sta :: AddressingMode -> CPU r ()
sta = storeRegisterInMemory A

-- | Store the value of the X register in memory
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#STX
stx :: AddressingMode -> CPU r ()
stx = storeRegisterInMemory X

-- | Store the value of the Y register in memory
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#STY
sty :: AddressingMode -> CPU r ()
sty = storeRegisterInMemory Y

storeRegisterInMemory :: Register -> AddressingMode -> CPU r ()
storeRegisterInMemory reg mode = do
    addr <- getOperandAddr mode
    value <- getRegister reg
    withBus $ writeByte value addr
