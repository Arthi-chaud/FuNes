module Nes.CPU.Instructions.Logic (and, ora, eor) where

import Data.Bits (Bits ((.|.)), (.&.), (.^.))
import Nes.CPU.Instructions.Addressing (AddressingMode, getOperandAddr)
import Nes.CPU.Instructions.After (setZeroAndNegativeFlags)
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory
import Prelude hiding (and)

-- | Register A = Register A & _value in memory_
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#AND
and :: AddressingMode -> CPU r ()
and = applyLogicOnRegisterA (.&.)

-- | Register A = Register A | _value in memory_
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#ORA
ora :: AddressingMode -> CPU r ()
ora = applyLogicOnRegisterA (.|.)

-- | Register A = Register A ^ _value in memory_
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#EOR
eor :: AddressingMode -> CPU r ()
eor = applyLogicOnRegisterA (.^.)

applyLogicOnRegisterA :: (Byte -> Byte -> Byte) -> AddressingMode -> CPU r ()
applyLogicOnRegisterA op mode = do
    value <- getOperandAddr mode >>= withBus . readByte
    regA <- getRegister A
    let res = regA `op` value
    setZeroAndNegativeFlags res
    setRegister A res
