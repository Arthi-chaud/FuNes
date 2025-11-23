module Nes.Bus.State (
    BusState (..),
    newBusState,
    modifyPPUState,
) where

import Nes.APU.State (APUState, newAPUState)
import Nes.APU.State.Filter.Constants (Sample)
import Nes.APU.State.Filter.Thread (FilterThread)
import Nes.Controller
import Nes.Internal
import Nes.Interrupt
import Nes.Memory
import Nes.Memory.Unsafe ()
import Nes.PPU.Constants
import Nes.PPU.Pointers (PPUPointers, newPPUPointers)
import Nes.PPU.State (PPUState, newPPUState)
import Nes.Rom (Rom (..))

-- | Interface for the CPU that allows it to read/write to RAM
data BusState = BusState
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
    , onNewFrame :: BusState -> IO ()
    , pollControls :: Controller -> IO Controller
    , dataBus :: {-# UNPACK #-} !Byte
    -- ^ Last read/written byte
    , apuState :: !APUState
    , cpuInterrupt :: {-# UNPACK #-} !InterruptStatus
    }

newBusState ::
    Rom ->
    -- | Callback on new frame
    (BusState -> IO ()) ->
    -- | Callback to poll controller inputs
    (Controller -> IO Controller) ->
    -- | Callback when a sample is ready
    (Sample -> IO ()) ->
    -- | Callback when a cycle ends
    (Double -> Int -> IO (Double, Int)) ->
    FilterThread ->
    IO BusState
newBusState cartridge onNewFrame pollControls pushSample cycleCallback filterThread = do
    cpuVram <- callocForeignPtr vramSize
    ppuPointers <- newPPUPointers
    let controller = newController
        ppuState = newPPUState (mirroring cartridge)
        cycles = 0
        unsleptCycles = 0
        lastSleepTime = 0
        dataBus = 0
        apuState = newAPUState pushSample filterThread
        cpuInterrupt = MkIS Nothing False
    return $ BusState{..}

modifyPPUState :: (PPUState -> PPUState) -> BusState -> BusState
modifyPPUState f bus = bus{ppuState = f $ ppuState bus}
