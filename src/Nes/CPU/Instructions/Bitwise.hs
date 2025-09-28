module Nes.CPU.Instructions.Bitwise (bit, and, ora, eor, rol, ror, asl, lsr, slo) where

import Control.Monad
import Data.Bits (Bits (setBit, shiftL, testBit, (.|.)), shiftR, (.&.), (.^.))
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.After
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory
import Prelude hiding (and)

bit :: AddressingMode -> CPU r ()
bit mode = do
    value <- getOperandAddr mode >>= flip readByte ()
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
    value <- getOperandAddr mode >>= flip readByte ()
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
rotate f setCarry mode =
    withOperand
        mode
        ( \value -> do
            res <- f value <$> getStatusFlag Carry
            setZeroAndNegativeFlags res
            modifyCPUState $ setCarry value
            return res
        )
        >> when (mode == AbsoluteX) tickOnce

-- | Arithmetic Shift Left
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#ASL
asl :: AddressingMode -> CPU r ()
asl mode = asl_ mode >> when (mode == AbsoluteX) tickOnce

asl_ :: AddressingMode -> CPU r Byte
asl_ mode = withOperand mode $ \value -> do
    let carry = testBit value 7
        res = shiftL value 1
    setStatusFlag' Carry carry
    setZeroAndNegativeFlags res
    return res

-- | Logical Shift Right
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#LSR
lsr :: AddressingMode -> CPU r ()
lsr mode =
    withOperand
        mode
        ( \value -> do
            let carry = testBit value 0
                res = shiftR value 1
            setStatusFlag' Carry carry
            setZeroAndNegativeFlags res
            return res
        )
        >> when (mode == AbsoluteX) tickOnce

-- | (Unofficial) Equivalent to ASL value then ORA value, except supporting more addressing modes
slo :: AddressingMode -> CPU r ()
slo mode = do
    value <- asl_ mode
    regA <- getRegister A
    let res = regA .|. value
    setZeroAndNegativeFlags res
    setRegister A res

withOperand :: AddressingMode -> (Byte -> CPU r Byte) -> CPU r Byte
withOperand Accumulator f = do
    a <- getRegister A
    res <- f a
    setRegister A res
    tickOnce
    return res
withOperand mode f = do
    addr <- getOperandAddr mode
    value <- readByte addr ()
    res <- f value
    -- https://www.nesdev.org/wiki/Cycle_counting
    --  it takes 1 extra cycle to modify the value
    tickOnce
    writeByte res addr ()
    return res
