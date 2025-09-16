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
    | ZeroPage
    | ZeroPageX
    | ZeroPageY
    | Absolute
    | AbsoluteX
    | AbsoluteY
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
    Immediate -> do
        arg <- getPCAsAddr
        incrementPC
        return arg
    ZeroPage -> do
        addr <- getPCAsAddr
        arg <- withBus $ readByte addr
        incrementPC
        return $ fromIntegral arg
    ZeroPageX -> zeroPageAddressing registerX
    ZeroPageY -> zeroPageAddressing registerY
    Absolute -> do
        addr <- getPCAsAddr
        arg <- withBus $ readAddr addr
        incrementPC >> incrementPC
        return arg
    AbsoluteX -> absoluteAddressing registerX
    AbsoluteY -> absoluteAddressing registerY
    IndirectX -> indirectAddressing registerX
    IndirectY -> indirectAddressing registerY
    None -> fail $ printf "Mode not supported: %s" $ show None

zeroPageAddressing :: (CPUState -> Byte) -> CPU r Addr
zeroPageAddressing getter = do
    pos <- getPCAsAddr >>= (withBus . readByte)
    incrementPC
    withCPUState $ fromIntegral . (+ pos) . getter

absoluteAddressing :: (CPUState -> Byte) -> CPU r Addr
absoluteAddressing getter = do
    base <- getPCAsAddr >>= (withBus . readAddr)
    incrementPC >> incrementPC
    withCPUState $ (+ base) . fromIntegral . getter

indirectAddressing :: (CPUState -> Byte) -> CPU r Addr
indirectAddressing getter = do
    base <- getPCAsAddr >>= (withBus . readByte)
    incrementPC
    ptr <- withCPUState $ (+ base) . getter
    low <- fromIntegral <$> withBus (readByte (fromIntegral ptr))
    high <- fromIntegral <$> withBus (readByte (fromIntegral (ptr + 1)))
    return $ shiftL 8 high .|. low
