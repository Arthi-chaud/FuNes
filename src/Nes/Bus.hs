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
import Nes.Internal
import Nes.Memory
import Nes.Memory.Unsafe ()
import Nes.PPU.Pointers (PPUPointers, newPPUPointers)
import Nes.PPU.State (PPUState, newPPUState)
import Nes.Rom (Rom (..))

-- | Interface for the CPU that allows it to read/write to RAM
data Bus = Bus
    { cpuVram :: MemoryPointer
    -- ^ Pointer to writeable memory
    , cartridge :: Rom
    -- ^ Read-only memory, see 'Rom'
    , cycles :: Integer
    -- ^ The number of ellapsed cycles
    , cycleCallback :: IO ()
    -- ^ The function executed on every tick (e.g. sleep)
    , ppuState :: PPUState
    -- ^ The state of the PPU
    , ppuPointers :: PPUPointers
    -- ^ Memory dedicated to PPU
    }

newBus :: Rom -> IO Bus
newBus rom_ = do
    fptr <- callocVram
    ppuPtrs <- newPPUPointers (chrRom rom_)
    let ppuSt = newPPUState (mirroring rom_)
    return $ Bus (castForeignPtr fptr) rom_ 0 (pure ()) ppuSt ppuPtrs
  where
    vramSize = 2048
    callocVram = callocForeignPtr vramSize
