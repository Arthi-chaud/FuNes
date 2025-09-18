module Nes.CPU.Instructions.Arith (adc, sbc) where

import Data.Bits
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.After (setZeroAndNegativeFlags)
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory

-- | Regisiter A + (_value in memory_) + Carry
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#ADC
adc :: AddressingMode -> CPU r ()
adc mode = do
    value <- getOperandAddr mode >>= withBus . readByte
    carry <- fromIntegral . fromEnum <$> getStatusFlag Carry
    res <- addToRegisterA value carry
    setRegister A res
    setZeroAndNegativeFlags res

-- | Regisiter A - (_value in memory_) - (1 - Carry)
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#SBC
sbc :: AddressingMode -> CPU r ()
sbc mode = do
    value <- getOperandAddr mode >>= withBus . readByte
    carry <- fromIntegral . fromEnum <$> getStatusFlag Carry
    res <- addToRegisterA (-value) (-(1 - carry))
    setRegister A res
    setZeroAndNegativeFlags res

-- | Does the computation and sets Carry and Overflow accordingly
--
-- Source: https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch3.3/src/cpu.rs#L261
addToRegisterA :: Byte -> Byte -> CPU r Byte
addToRegisterA value carry = do
    regA <- byteToInt <$> getRegister A
    let sumInt = regA + byteToInt value + byteToInt carry
    setStatusFlag' Carry $ sumInt > 0xff
    let sumByte = fromIntegral sumInt :: Byte
    setStatusFlag' Overflow $ (value `xor` sumByte) .&. (sumByte `xor` fromIntegral regA) .&. 0x80 /= 0
    setRegister A sumByte
    return sumByte
