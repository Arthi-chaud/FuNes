{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Nes.Bus (
    -- * Bus
    Bus (..),
    newBus,
    modifyPPUState,
) where

import Nes.APU.State (APUState, newAPUState)
import Nes.Bus.SideEffect (CPUSideEffect)
import Nes.Controller
import Nes.Internal
import Nes.Memory
import Nes.Memory.Unsafe ()
import Nes.PPU.Constants
import Nes.PPU.Pointers (PPUPointers, newPPUPointers)
import Nes.PPU.State (PPUState, newPPUState)
import Nes.Rom (Rom (..))

-- | Interface for the CPU that allows it to read/write to RAM
data Bus = Bus
    { cpuVram :: {-# UNPACK #-} !MemoryPointer
    -- ^ Pointer to writeable memory
    , cartridge :: !Rom
    -- ^ Read-only memory, see 'Rom'
    , controller :: !Controller
    -- ^ Aka Joypad
    , cycles :: {-# UNPACK #-} !Integer
    , unsleptCycles :: {-# UNPACK #-} !Int
    -- ^ The number of cycles that we need to call 'threadDelay' for
    , cycleCallback :: Double -> Int -> IO (Double, Int)
    -- ^ The function to call 'threadDelay' according to 'unsleptCycles' (> 'unsleptCyclesThreshold')
    -- The return value is the new number of unslept cycles
    , lastSleepTime :: {-# UNPACK #-} !Double
    , ppuState :: !PPUState
    -- ^ The state of the PPU
    , ppuPointers :: !PPUPointers
    -- ^ Memory dedicated to PPU
    , onNewFrame :: Bus -> IO Bus
    , dataBus :: {-# UNPACK #-} !Byte
    -- ^ Last read/written byte
    , apuState :: !APUState
    , cpuSideEffect :: {-# UNPACK #-} !CPUSideEffect
    }

newBus :: Rom -> (Bus -> IO Bus) -> (Float -> IO ()) -> (Double -> Int -> IO (Double, Int)) -> IO Bus
newBus rom_ onNewFrame_ pushSample_ tickCallback_ = do
    fptr <- callocForeignPtr vramSize
    ppuPtrs <- newPPUPointers
    let ppuSt = newPPUState (mirroring rom_)
    return $
        Bus
            fptr
            rom_
            newController
            0
            0
            tickCallback_
            0
            ppuSt
            ppuPtrs
            onNewFrame_
            0
            (newAPUState pushSample_)
            mempty

modifyPPUState :: (PPUState -> PPUState) -> Bus -> Bus
modifyPPUState f bus = bus{ppuState = f $ ppuState bus}
