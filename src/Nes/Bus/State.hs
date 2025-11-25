{-# LANGUAGE TemplateHaskell #-}

module Nes.Bus.State (
    BusState (..),
    newBusState,

    -- * Lenses
    cpuVram,
    cartridge,
    controller,
    cycles,
    unsleptCycles,
    cycleCallback,
    lastSleepTime,
    ppuState,
    ppuPointers,
    onNewFrame,
    dataBus,
    apuState,
    pollControls,
    cpuInterrupt,
) where

import Control.Lens (makeLenses)
import Nes.APU.State (APUState, newAPUState)
import Nes.APU.State.Filter.Constants (Sample)
import Nes.APU.State.Filter.Thread (FilterThread)
import Nes.Controller
import Nes.Interrupt
import Nes.Memory
import Nes.Memory.Internal
import Nes.Memory.Unsafe ()
import Nes.PPU.Constants
import Nes.PPU.Pointers (PPUPointers, newPPUPointers)
import Nes.PPU.State (PPUState, newPPUState)
import Nes.Rom (Rom (..))

-- | Interface for the CPU that allows it to read/write to RAM
data BusState = BusState
    { _cpuVram :: {-# UNPACK #-} !MemoryPointer
    -- ^ Pointer to writeable memory
    , _cartridge :: !Rom
    -- ^ Read-only memory, see 'Rom'
    , _controller :: !ControllerState
    -- ^ Aka Joypad
    , _cycles :: {-# UNPACK #-} !Integer
    , _unsleptCycles :: {-# UNPACK #-} !Int
    -- ^ The number of cycles that we need to call 'threadDelay' for
    , _cycleCallback :: Double -> Int -> IO (Double, Int)
    -- ^ The function to call 'threadDelay' according to 'unsleptCycles' (> 'unsleptCyclesThreshold')
    -- The return value is the new number of unslept cycles
    , _lastSleepTime :: {-# UNPACK #-} !Double
    , _ppuState :: !PPUState
    -- ^ The state of the PPU
    , _ppuPointers :: !PPUPointers
    -- ^ Memory dedicated to PPU
    , _onNewFrame :: BusState -> IO ()
    , _pollControls :: ControllerState -> IO ControllerState
    , _dataBus :: {-# UNPACK #-} !Byte
    -- ^ Last read/written byte
    , _apuState :: !APUState
    , _cpuInterrupt :: {-# UNPACK #-} !InterruptStatus
    }

newBusState ::
    Rom ->
    -- | Callback on new frame
    (BusState -> IO ()) ->
    -- | Callback to poll controller inputs
    (ControllerState -> IO ControllerState) ->
    -- | Callback when a sample is ready
    (Sample -> IO ()) ->
    -- | Callback when a cycle ends
    (Double -> Int -> IO (Double, Int)) ->
    FilterThread ->
    IO BusState
newBusState _cartridge _onNewFrame _pollControls pushSample _cycleCallback filterThread = do
    _cpuVram <- callocForeignPtr vramSize
    _ppuPointers <- newPPUPointers
    let _controller = newControllerState
        _ppuState = newPPUState (mirroring _cartridge)
        _cycles = 0
        _unsleptCycles = 0
        _lastSleepTime = 0
        _dataBus = 0
        _apuState = newAPUState pushSample filterThread
        _cpuInterrupt = MkIS Nothing False
    return $ BusState{..}

makeLenses ''BusState
