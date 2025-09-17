module Nes.CPU.Instructions.IN (inc, inx, iny) where

import Nes.CPU.Instructions.Addressing (AddressingMode, getOperandAddr)
import Nes.CPU.Instructions.After
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory

-- | Increment value in memory
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#INC
inc :: AddressingMode -> CPU r ()
inc mode = do
    addr <- getOperandAddr mode
    res <- (+ 1) <$> withBus (readByte addr)
    withBus $ writeByte res addr
    setZeroAndNegativeFlags res

-- | Increment the value of the X register
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#INX
inx :: CPU r ()
inx = incrementRegister X

-- | Increment the value of the Y register
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#INY
iny :: CPU r ()
iny = incrementRegister Y

incrementRegister :: Register -> CPU r ()
incrementRegister reg = do
    newRegY <- (+ 1) <$> getRegister reg
    setRegister reg newRegY
    setZeroAndNegativeFlags newRegY
