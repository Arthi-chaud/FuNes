module Nes.CPU.Instructions.Addressing (AddressingMode (..), getOperandAddr) where

import Data.Bits (Bits (shiftL, (.|.)))
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory
import Text.Printf (printf)

-- | Mode that say how to get an Op code's parameter
--
-- https://www.nesdev.org/obelisk-6502-guide/addressing.html#IMM
data AddressingMode
    = Immediate
    | Accumulator
    | Relative
    | ZeroPage
    | ZeroPageX
    | ZeroPageY
    | Absolute
    | AbsoluteX
    | AbsoluteY
    | Indirect
    | IndirectX
    | IndirectY
    | None
    deriving (Eq, Show)

-- | Gives the address of the current op code's parameter
--
-- Will shift PC accordingly
--
-- Source: https://bugzmanov.github.io/nes_ebook/chapter_3_2.html
getOperandAddr :: AddressingMode -> CPU r Addr
getOperandAddr = \case
    Accumulator -> fail "Do not use this function when the mode is Accumulator"
    Immediate -> do
        arg <- getPC
        incrementPC
        return arg
    Relative -> do
        addr <- getPC
        arg <- withBus $ readByte addr
        return $ addr + byteToAddr arg
    ZeroPage -> do
        arg <- getPC >>= withBus . readByte
        incrementPC
        return $ byteToAddr arg
    ZeroPageX -> zeroPageAddressing registerX
    ZeroPageY -> zeroPageAddressing registerY
    Absolute -> do
        addr <- getPC >>= (withBus . readAddr)
        incrementPC >> incrementPC
        return addr
    AbsoluteX -> absoluteAddressing registerX
    AbsoluteY -> absoluteAddressing registerY
    -- No need to increment PC here. Mode is only used by jmp
    Indirect -> getPC >>= withBus . readAddr >>= withBus . readAddr
    IndirectX -> indirectAddressing registerX
    IndirectY -> indirectAddressing registerY
    None -> fail $ printf "Mode not supported: %s" $ show None

zeroPageAddressing :: (CPUState -> Byte) -> CPU r Addr
zeroPageAddressing getter = do
    pos <- getPC >>= (withBus . readByte)
    incrementPC
    withCPUState $ byteToAddr . (+ pos) . getter

absoluteAddressing :: (CPUState -> Byte) -> CPU r Addr
absoluteAddressing getter = do
    base <- getPC >>= (withBus . readAddr)
    incrementPC >> incrementPC
    withCPUState $ (+ base) . byteToAddr . getter

indirectAddressing :: (CPUState -> Byte) -> CPU r Addr
indirectAddressing getter = do
    base <- getPC >>= (withBus . readByte)
    incrementPC
    ptr <- withCPUState $ (+ base) . getter
    low <- withBus (readByte (byteToAddr ptr))
    high <- byteToAddr <$> withBus (readByte (byteToAddr (ptr + 1)))
    return $ shiftL high 8 .|. byteToAddr low
