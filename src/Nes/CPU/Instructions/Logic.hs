module Nes.CPU.Instructions.Logic (bit, and, ora, eor, rol, ror) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bits (Bits (setBit, shiftL, testBit, (.|.)), shiftR, (.&.), (.^.))
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.After
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory
import Prelude hiding (and)

bit :: AddressingMode -> CPU r ()
bit mode = do
    value <- getOperandAddr mode >>= withBus . readByte
    regA <- getRegister A
    let res = regA .&. value
    setStatusFlag' Zero $ res == 0
    setStatusFlag' Overflow $ testBit value 6
    setStatusFlag' Negative $ testBit value 7

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

-- | Rotate left
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#ROL
rol :: AddressingMode -> CPU r ()
rol =
    rotate
        ( \value carry ->
            let shifted = shiftL value 1
             in if carry then setBit shifted 0 else shifted
        )
        (\byte -> setStatusFlagPure' Carry (testBit byte 7))

-- | Rotate right
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#ROR
ror :: AddressingMode -> CPU r ()
ror =
    rotate
        ( \value carry ->
            let shifted = shiftR value 1
             in if carry then setBit shifted 7 else shifted
        )
        (\byte -> setStatusFlagPure' Carry (testBit byte 0))

rotate :: (Byte -> Bool -> Byte) -> (Byte -> CPUState -> CPUState) -> AddressingMode -> CPU r ()
rotate f setCarry mode = do
    value <- case mode of
        Accumulator -> getRegister A
        _ -> getOperandAddr mode >>= withBus . readByte
    res <- f value <$> getStatusFlag Carry
    setZeroAndNegativeFlags res
    modifyCPUState $ setCarry value
    if mode == Accumulator
        then setRegister A res
        else getOperandAddr mode >>= withBus . writeByte res
