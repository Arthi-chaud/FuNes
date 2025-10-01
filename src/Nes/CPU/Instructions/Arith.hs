{-# LANGUAGE ScopedTypeVariables #-}

module Nes.CPU.Instructions.Arith (
    -- * Operate on register A with carry
    adc,
    sbc,

    -- * Increment
    inc,
    inx,
    iny,
    isb,

    -- * Decrement
    dec,
    dex,
    dey,

    -- * Internal
    addToRegisterA,
) where

import Control.Monad
import Data.Bits
import Data.Word
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
    value <- getOperandAddr mode >>= flip readByte ()
    addToRegisterA value

-- | Regisiter A - (_value in memory_) - (1 - Carry)
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#SBC
sbc :: AddressingMode -> CPU r ()
sbc mode = do
    value <- getOperandAddr mode >>= flip readByte ()
    addToRegisterA (negateByte value)

-- | Does the computation and sets Carry and Overflow accordingly
--
-- Source: https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch3.3/src/cpu.rs#L261
addToRegisterA :: Byte -> CPU r ()
addToRegisterA value = do
    carry <- withCPUState $ getStatusFlag Carry
    regA :: Word16 <- fromIntegral . unByte <$> withCPUState (getRegister A)
    let sumInt =
            regA
                + (fromIntegral . unByte $ value)
                + (if carry then 1 else 0)
    modifyCPUState $ setStatusFlag' Carry $ sumInt > 0x00ff
    let sumByte = fromIntegral sumInt :: Byte
    modifyCPUState $ setStatusFlag' Overflow $ (value `xor` sumByte) .&. (sumByte `xor` fromIntegral regA) .&. 0x80 /= 0
    modifyCPUState $ setRegister A sumByte
    setZeroAndNegativeFlags sumByte

-- | Increment value in memory
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#INC
inc :: AddressingMode -> CPU r ()
inc mode = do
    void $ modifyValueInMemory (+ 1) mode
    when (mode == AbsoluteX) tickOnce

-- | (Unofficial) Equivalent to INC value then SBC value
--
-- Aka ISC
isb :: AddressingMode -> CPU r ()
isb mode = do
    byte <- modifyValueInMemory (+ 1) mode
    addToRegisterA (negateByte byte)

-- | Decrement value in memory
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#DEC
dec :: AddressingMode -> CPU r ()
dec mode = do
    void $ modifyValueInMemory (+ (-1)) mode
    when (mode == AbsoluteX) tickOnce

modifyValueInMemory :: (Byte -> Byte) -> AddressingMode -> CPU r Byte
modifyValueInMemory f mode = do
    addr <- getOperandAddr mode
    res <- f <$> readByte addr ()
    tickOnce -- Is a Read-modify-write operation, we add one tick
    writeByte res addr ()
    setZeroAndNegativeFlags res
    return res

-- | Decrement X register
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#DEX
dex :: CPU r ()
dex = decrementRegister X

-- | Decrement Y register
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#DEY
dey :: CPU r ()
dey = decrementRegister Y

decrementRegister :: Register -> CPU r ()
decrementRegister reg = do
    res <- (\y -> y - 1) <$> withCPUState (getRegister reg)
    modifyCPUState $ setRegister reg res
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
    newRegY <- (+ 1) <$> withCPUState (getRegister reg)
    modifyCPUState (setRegister reg newRegY)
    setZeroAndNegativeFlags newRegY
