module Nes.CPU.Instructions.Bitwise (
    -- * Test bit
    bit,

    -- * Bit logic
    and,
    ora,
    eor,

    -- * Rotate
    rol,
    ror,
    rla,
    asl,
    lsr,

    -- * Unofficial
    anc,
    sre,
    slo,
    alr,

    -- * Internal
    rol_,
    ror_,
) where

import Control.Monad
import Data.Bits (Bits (setBit, shiftL, testBit, (.|.)), shiftR, (.&.), (.^.))
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.After
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.FlagRegister
import Nes.Memory
import Prelude hiding (and)

-- |  test if one or more bits are set in a target memory location
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#BIT
bit :: AddressingMode -> CPU r ()
bit mode = do
    value <- getOperandAddr mode >>= flip readByte ()
    regA <- withCPUState $ getRegister A
    let res = regA .&. value
    modifyCPUState $
        modifyStatusRegister $
            setFlag' Zero (res == 0)
                . setFlag' Overflow (testBit value 6)
                . setFlag' Negative (testBit value 7)

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
    void $ modifyRegisterA (`op` value)

-- | Rotate left
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#ROL
rol :: AddressingMode -> CPU r ()
rol mode = do
    _ <- rol_ mode
    when (mode == AbsoluteX) tickOnce

rol_ :: AddressingMode -> CPU r Byte
rol_ =
    rotate
        ( \value carry ->
            let shifted = shiftL value 1
             in if carry then setBit shifted 0 else shifted
        )
        (\byte -> modifyStatusRegister $ setFlag' Carry (testBit byte 7))

-- | Rotate right
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#ROR
ror :: AddressingMode -> CPU r ()
ror mode = do
    _ <- ror_ mode
    when (mode == AbsoluteX) tickOnce

ror_ :: AddressingMode -> CPU r Byte
ror_ =
    rotate
        ( \value carry ->
            let shifted = shiftR value 1
             in if carry then setBit shifted 7 else shifted
        )
        (\byte -> modifyStatusRegister $ setFlag' Carry (testBit byte 0))

rotate :: (Byte -> Bool -> Byte) -> (Byte -> CPUState -> CPUState) -> AddressingMode -> CPU r Byte
rotate f setCarry mode =
    withOperand
        mode
        ( \value -> do
            res <- f value <$> withCPUState (getFlag Carry . status)
            setZeroAndNegativeFlags res
            modifyCPUState $ setCarry value
            return res
        )

rla :: AddressingMode -> CPU r ()
rla mode = do
    value <- rol_ mode
    void $ modifyRegisterA (value .&.)

anc :: AddressingMode -> CPU r ()
anc =
    and >=> \() -> do
        isNeg <- withCPUState $ getFlag Negative . status
        modifyCPUState $ modifyStatusRegister (setFlag' Carry isNeg)

-- | Arithmetic Shift Left
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#ASL
asl :: AddressingMode -> CPU r ()
asl mode = asl_ mode >> when (mode == AbsoluteX) tickOnce

asl_ :: AddressingMode -> CPU r Byte
asl_ mode = withOperand mode $ \value -> do
    let carry = testBit value 7
        res = shiftL value 1
    modifyCPUState $ modifyStatusRegister $ setFlag' Carry carry
    setZeroAndNegativeFlags res
    return res

-- | Logical Shift Right
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#LSR
lsr :: AddressingMode -> CPU r ()
lsr mode = lsr_ mode >> when (mode == AbsoluteX) tickOnce

lsr_ :: AddressingMode -> CPU r Byte
lsr_ mode =
    withOperand
        mode
        ( \value -> do
            let carry = testBit value 0
                res = shiftR value 1
            modifyCPUState $ modifyStatusRegister $ setFlag' Carry carry
            setZeroAndNegativeFlags res
            return res
        )

-- | (Unofficial) Equivalent to LSR and XOR
sre :: AddressingMode -> CPU r ()
sre mode = do
    value <- lsr_ mode
    void $ modifyRegisterA (.^. value)

-- | (Unofficial) Equivalent to ASL value then ORA value, except supporting more addressing modes
slo :: AddressingMode -> CPU r ()
slo mode = do
    value <- asl_ mode
    void $ modifyRegisterA (value .|.)

-- | (Unofficial) AND + LSR
--
-- Source: https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch8/src/cpu.rs#L1073
alr :: AddressingMode -> CPU r ()
alr mode = and mode >> lsr Accumulator

withOperand :: AddressingMode -> (Byte -> CPU r Byte) -> CPU r Byte
withOperand Accumulator f = do
    a <- withCPUState $ getRegister A
    res <- f a
    modifyCPUState $ setRegister A res
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

modifyRegisterA :: (Byte -> Byte) -> CPU r Byte
modifyRegisterA f = do
    regA <- withCPUState $ getRegister A
    let res = f regA
    setZeroAndNegativeFlags res
    modifyCPUState $ setRegister A res
    return res
