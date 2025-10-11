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
    , cartridge :: {-# UNPACK #-} !Rom
    -- ^ Read-only memory, see 'Rom'
    , controller :: {-# UNPACK #-} !Controller
    -- ^ Aka Joypad
    , cycles :: {-# UNPACK #-} !Integer
    -- ^ The number of ellapsed cycles
    , cycleCallback :: Int -> IO ()
    -- ^ The function executed on tick (the arg is the number of ellapsed ticks)
    , ppuState :: {-# UNPACK #-} !PPUState
    -- ^ The state of the PPU
    , ppuPointers :: {-# UNPACK #-} !PPUPointers
    -- ^ Memory dedicated to PPU
    , onNewFrame :: Bus -> IO Bus
    }

newBus :: Rom -> (Bus -> IO Bus) -> (Int -> IO ()) -> IO Bus
newBus rom_ onNewFrame_ tickCallback_ = do
    fptr <- callocForeignPtr vramSize
    ppuPtrs <- newPPUPointers
    let ppuSt = newPPUState (mirroring rom_)
    return $
        Bus
            fptr
            rom_
            newController
            0
            tickCallback_
            ppuSt
            ppuPtrs
            onNewFrame_
