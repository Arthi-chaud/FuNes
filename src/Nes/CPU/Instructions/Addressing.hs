{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Nes.CPU.Instructions.Addressing (
    AddressingMode (..),
    getOperandAddr,
    getOperandAddr',
    getOperandSize,
) where

import Control.Monad
import Data.Bits
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
-- Will shift PC accordingly and ticks once when page is crossed
--
-- Source: https://bugzmanov.github.io/nes_ebook/chapter_3_2.html
getOperandAddr :: AddressingMode -> CPU r Addr
getOperandAddr mode = do
    (res, crosses) <- getOperandAddr' mode
    when (crosses && mode /= IndirectY && mode /= AbsoluteY && mode /= AbsoluteX) tickOnce
    return res

-- | Gives the address of the current op code's parameter
--
-- Will shift PC accordingly, but does not tick when page is crossed
getOperandAddr' :: AddressingMode -> CPU r (Addr, Bool)
getOperandAddr' mode = do
    (res, crosses) <- getOperandAddr'' mode
    let offset = Addr $ fromIntegral $ getOperandSize mode
    modifyCPUState $ \st -> st{programCounter = programCounter st + offset}
    return (res, crosses)

-- | Gives the address of the current op code's parameter
--
-- Does not shift the PC, does not tick when page is crossed by tick when reading bytes
getOperandAddr'' :: AddressingMode -> CPU r (Addr, Bool)
getOperandAddr'' = \case
    Accumulator -> fail "Do not use this function when the mode is Accumulator"
    Immediate -> (,False) <$> getPC
    Relative -> do
        pc <- getPC
        offset <- readByte pc ()
        let intPC = addrToInt pc
            -- Note we need to wrap the unsinged word into a signed value
            -- See https://www.nesdev.org/wiki/Instruction_reference#BPL
            signedOffset = fromIntegral (fromIntegral (unByte offset) :: Int8)
            res = Addr $ fromIntegral $ intPC + 1 + signedOffset
        return (res, crossesPage (fromIntegral $ intPC + 1) res)
    ZeroPage -> do
        arg <- getPC >>= flip readByte ()
        return (byteToAddr arg, False)
    ZeroPageX -> zeroPageAddressing registerX
    ZeroPageY -> zeroPageAddressing registerY
    Absolute -> do
        pc <- getPC
        addr <- readAddr pc ()
        return (addr, False)
    AbsoluteX -> absoluteAddressing registerX
    AbsoluteY -> absoluteAddressing registerY
    -- No need to increment PC here. Mode is only used by jmp
    Indirect -> do
        pc <- getPC
        addr1 <- readAddr pc ()
        addr2 <- readAddr addr1 ()
        return (addr2, False)
    IndirectX -> do
        -- Note: we do not convert the ptr to an Addr because we need the overflow to happen
        base <- getPC >>= flip readByte ()
        ptr <- withCPUState $ (+ base) . registerX
        low <- readByte (byteToAddr ptr) ()
        high <- readByte (byteToAddr (ptr + 1)) ()
        let res = bytesToAddr low high
        return (res, crossesPage (byteToAddr ptr) res)
    IndirectY -> do
        ptr <- getPC >>= flip readByte ()
        low <- readByte (byteToAddr ptr) ()
        high <- readByte (byteToAddr (ptr + 1)) ()
        y <- withCPUState $ getRegister Y
        let derefBase = bytesToAddr low high
            deref = derefBase + byteToAddr y
            crosses = crossesPage deref derefBase
        when crosses $ do
            let bogusAddr = (byteToAddr high `shiftL` 8) .|. byteToAddr (y + low)
            _ <- readByte bogusAddr ()
            return ()
        return (deref, crossesPage deref derefBase)
    None -> fail $ printf "Mode not supported: %s" $ show None

{-# INLINE zeroPageAddressing #-}
zeroPageAddressing :: (CPUState -> Byte) -> CPU r (Addr, Bool)
zeroPageAddressing getter = do
    pos <- getPC >>= flip readByte ()
    regVal <- withCPUState getter
    tickOnce
    return (byteToAddr $ pos + regVal, False)

{-# INLINE absoluteAddressing #-}
absoluteAddressing :: (CPUState -> Byte) -> CPU r (Addr, Bool)
absoluteAddressing getter = do
    base <- getPC >>= flip readAddr ()
    offset <- withCPUState getter
    let addr = byteToAddr offset + base
    let crosses = crossesPage base addr
    -- Dummy read
    when crosses $ do
        let bogusAddr = (base .&. 0xff00) .|. byteToAddr (offset + unsafeAddrToByte (base .&. 0xff))
        _ <- readByte bogusAddr ()
        return ()
    --
    return (addr, crosses)

{-# INLINE crossesPage #-}
crossesPage :: Addr -> Addr -> Bool
crossesPage (Addr addr1) (Addr addr2) = addr1 .&. 0xFF00 /= addr2 .&. 0xFF00

{-# INLINE getOperandSize #-}
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
