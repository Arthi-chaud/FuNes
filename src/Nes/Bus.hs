{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Nes.Bus (
    -- * Bus
    Bus (..),
    newBus,
) where

import Foreign
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
    { cpuVram :: MemoryPointer
    -- ^ Pointer to writeable memory
    , cartridge :: Rom
    -- ^ Read-only memory, see 'Rom'
    , controller :: Controller
    -- ^ Aka Joypad
    , cycles :: Integer
    -- ^ The number of ellapsed cycles
    , cycleCallback :: Int -> IO ()
    -- ^ The function executed on tick (the arg is the number of ellapsed ticks)
    , ppuState :: PPUState
    -- ^ The state of the PPU
    , ppuPointers :: PPUPointers
    -- ^ Memory dedicated to PPU
    , onNewFrame :: Bus -> IO Bus
    }

newBus :: Rom -> (Bus -> IO Bus) -> (Int -> IO ()) -> IO Bus
newBus rom_ onNewFrame_ tickCallback_ = do
    fptr <- callocForeignPtr vramSize
    ppuPtrs <- newPPUPointers (chrRom rom_)
    let ppuSt = newPPUState (mirroring rom_)
    return $
        Bus
            (castForeignPtr fptr)
            rom_
            newController
            0
            tickCallback_
            ppuSt
            ppuPtrs
            onNewFrame_
