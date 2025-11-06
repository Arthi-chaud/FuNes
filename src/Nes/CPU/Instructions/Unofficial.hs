{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

-- | Unofficial instructions that are combinations of official ones
module Nes.CPU.Instructions.Unofficial (lax, sax, dcp, rra, sha, shx, shy, shs, lxa, axs) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bits
import Nes.Bus (Bus (cpuSideEffect))
import Nes.Bus.SideEffect (CPUSideEffectFlag (DMCDMA))
import Nes.CPU.Instructions.Access (lda)
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.After (setZeroAndNegativeFlags)
import Nes.CPU.Instructions.Arith (addToRegisterA)
import Nes.CPU.Instructions.Bitwise (ror_)
import Nes.CPU.Instructions.Transfer
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.FlagRegister
import Nes.Memory
import Text.Printf (printf)

-- Source: https://www.nesdev.org/wiki/Programming_with_unofficial_opcodes

-- | Equivalent to LDA then TAX, saves a couple of cycles
lax :: AddressingMode -> CPU r ()
lax mode = do
    addr <- getOperandAddr mode
    byte <- readByte addr ()
    modifyCPUState $
        setRegister A byte
            . setRegister X byte
    setZeroAndNegativeFlags byte

-- | Stores the bitwise AND of A and X. No flags are affected
sax :: AddressingMode -> CPU r ()
sax mode = do
    dest <- getOperandAddr mode
    a <- withCPUState $ getRegister A
    x <- withCPUState $ getRegister X
    let res = a .&. x
    writeByte res dest ()

-- | Equivalent to DEC value then CMP value, except supporting more addressing modes
dcp :: AddressingMode -> CPU r ()
dcp mode = do
    addr <- getOperandAddr mode
    value <- (+ (-1)) <$> readByte addr ()
    writeByte value addr ()
    a <- withCPUState $ getRegister A
    tickOnce -- It's a Read-modify-write operation
    when (value <= a) $ modifyCPUState $ modifyStatusRegister (setFlag Carry)
    setZeroAndNegativeFlags (a - value)

-- | (Unofficial) ROR and ADC
rra :: AddressingMode -> CPU r ()
rra mode = do
    value <- ror_ mode
    addToRegisterA value

-- | Source: https://forums.nesdev.org/viewtopic.php?t=8107
shx :: AddressingMode -> CPU r ()
shx mode = do
    x <- withCPUState $ getRegister X
    operand <- getPC >>= flip readAddr ()
    sh' (const operand) (.&. x) (.&. x) mode

shy :: AddressingMode -> CPU r ()
shy mode = do
    y <- withCPUState $ getRegister Y
    operand <- getPC >>= flip readAddr ()
    sh' (const operand) (.&. y) (.&. y) mode

sha :: AddressingMode -> CPU r ()
sha mode = do
    a <- withCPUState $ getRegister A
    x <- withCPUState $ getRegister X
    y <- withCPUState $ getRegister Y
    sh' (\addr -> addr - byteToAddr y) (\h -> h .&. a .&. x) (\v -> v .&. a .&. x) mode

shs :: AddressingMode -> CPU r ()
shs mode = do
    a <- withCPUState $ getRegister A
    x <- withCPUState $ getRegister X
    operand <- getPC >>= flip readAddr ()
    let s = a .&. x
    modifyCPUState $ setRegister S s
    sh' (const operand) (\h -> h .&. a .&. x) (.&. s) mode

sh' ::
    (Addr -> Addr) -> -- Return value from which H will be extracted. Arg is operand addr
    (Byte -> Byte) -> -- Compute the high byte of the destination addr. Arg is H + 1
    (Byte -> Byte) -> -- Compute the value to write. Arg is H + 1
    AddressingMode ->
    CPU r ()
sh' getHigh getDestHigh getValue mode = do
    (originalDest, crosses) <- getOperandAddr' mode
    dmcDmaBefore <- withBusState $ getFlag DMCDMA . cpuSideEffect
    liftIO $ putStrLn $ "DMC Before: " ++ show dmcDmaBefore
    tickOnce
    dmcDmaAfter <- withBusState $ getFlag DMCDMA . cpuSideEffect
    liftIO $ putStrLn $ "DMC After: " ++ show dmcDmaAfter
    when (not dmcDmaBefore && dmcDmaAfter) $ liftIO $ putStrLn "DMC DMA Happened on fourth cycle"
    let ignoreH = False
    let high = if ignoreH then 0xff else 1 + unsafeAddrToByte ((getHigh originalDest) `shiftR` 8)
        destHigh = getDestHigh high
        value = getValue high
        dest =
            if crosses
                then ((byteToAddr destHigh) `shiftL` 8) .|. (originalDest .&. 0xff)
                else originalDest
    writeByte value dest ()

lxa :: AddressingMode -> CPU r ()
lxa = lda >=> const tax

axs :: AddressingMode -> CPU r ()
axs mode = do
    addr <- getOperandAddr mode
    byte <- readByte addr ()
    x <- withCPUState $ getRegister X
    a <- withCPUState $ getRegister A
    let xAndA = x .&. a
    let res = xAndA - byte
    modifyCPUState $
        modifyStatusRegister
            (setFlag' Carry (byte <= xAndA))
            . setRegister X res
    setZeroAndNegativeFlags res
