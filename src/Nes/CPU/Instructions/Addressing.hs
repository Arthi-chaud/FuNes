module Nes.CPU.Instructions.Addressing (AddressingMode (..), getOperandAddr) where

import Data.Int (Int8)
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
        pc <- getPC
        offset <- withBus $ readByte pc
        incrementPC
        let intPC = fromIntegral $ unAddr pc :: Int
            -- Note we need to wrap the unsinged word into a signed value
            -- See https://www.nesdev.org/wiki/Instruction_reference#BPL
            signedOffset = fromIntegral (fromIntegral (unByte offset) :: Int8)
        return $ Addr $ fromIntegral $ intPC + 1 + signedOffset
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
    pos <- byteToAddr <$> (getPC >>= (withBus . readByte))
    incrementPC
    regVal <- byteToAddr <$> withCPUState getter
    return $ pos + regVal

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
    high <- withBus (readByte (byteToAddr (ptr + 1)))
    return $ bytesToAddr low high
