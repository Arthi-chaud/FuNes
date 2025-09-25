{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Nes.CPU.Instructions.Addressing (
    AddressingMode (..),
    getOperandAddr,
    getOperandAddr',
    getOperandSize,
) where

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
getOperandAddr mode = do
    addr <- getOperandAddr' mode
    let offset = Addr $ fromIntegral $ getOperandSize mode
    modifyCPUState $ \st -> st{programCounter = programCounter st + offset}
    return addr

-- | Gives the address of the current op code's parameter
--
-- Does NOT shift PC
getOperandAddr' :: AddressingMode -> CPU r Addr
getOperandAddr' = \case
    Accumulator -> fail "Do not use this function when the mode is Accumulator"
    Immediate -> getPC
    Relative -> do
        pc <- getPC
        offset <- withBus $ readByte pc
        let intPC = fromIntegral $ unAddr pc :: Int
            -- Note we need to wrap the unsinged word into a signed value
            -- See https://www.nesdev.org/wiki/Instruction_reference#BPL
            signedOffset = fromIntegral (fromIntegral (unByte offset) :: Int8)
        return $ Addr $ fromIntegral $ intPC + 1 + signedOffset
    ZeroPage -> do
        arg <- getPC >>= withBus . readByte
        return $ byteToAddr arg
    ZeroPageX -> zeroPageAddressing registerX
    ZeroPageY -> zeroPageAddressing registerY
    Absolute -> getPC >>= (withBus . readAddr)
    AbsoluteX -> absoluteAddressing registerX
    AbsoluteY -> absoluteAddressing registerY
    -- No need to increment PC here. Mode is only used by jmp
    Indirect -> getPC >>= withBus . readAddr >>= withBus . readAddr
    IndirectX -> do
        base <- getPC >>= (withBus . readByte)
        ptr <- withCPUState $ (+ base) . registerX
        low <- withBus (readByte (byteToAddr ptr))
        high <- withBus (readByte (byteToAddr (ptr + 1)))
        return $ bytesToAddr low high
    IndirectY -> do
        ptr <- getPC >>= (withBus . readByte)
        low <- withBus (readByte (byteToAddr ptr))
        high <- withBus (readByte (byteToAddr (ptr + 1)))
        y <- getRegister Y
        return $ byteToAddr y + bytesToAddr low high
    None -> fail $ printf "Mode not supported: %s" $ show None

zeroPageAddressing :: (CPUState -> Byte) -> CPU r Addr
zeroPageAddressing getter = do
    pos <- getPC >>= (withBus . readByte)
    regVal <- withCPUState getter
    return $ byteToAddr $ pos + regVal

absoluteAddressing :: (CPUState -> Byte) -> CPU r Addr
absoluteAddressing getter = do
    base <- getPC >>= (withBus . readAddr)
    withCPUState $ (+ base) . byteToAddr . getter

getOperandSize :: AddressingMode -> Int
getOperandSize = \case
    Immediate -> 1
    Accumulator -> 0
    Relative -> 1
    ZeroPage -> 1
    ZeroPageX -> 1
    ZeroPageY -> 1
    Absolute -> 2
    AbsoluteX -> 2
    AbsoluteY -> 2
    Indirect -> 2
    IndirectX -> 1
    IndirectY -> 1
    None -> 0
