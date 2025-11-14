module Nes.APU.BusInterface.FrameCounter (write4017) where

import Control.Monad
import Data.Bits
import Nes.APU.Monad
import Nes.APU.State
import Nes.APU.State.FrameCounter
import Nes.APU.Tick
import Nes.Memory

-- | Callback when a byte is written to 0x4017 through the Bus
{-# INLINE write4017 #-}
write4017 :: Byte -> APU r ()
write4017 byte = do
    c <- withAPUState Nes.APU.State.cycle
    let seqMode = sequenceModeFromBool $ byte `testBit` 7
        inhibit = byte `testBit` 6
        delay = if even c then 4 else 3
    modifyAPUState $
        modifyFrameCounter $
            \fc -> fc{sequenceMode = seqMode, inhibitInterrupt = inhibit, delayedWriteSideEffectCycle = Just delay}
    -- If the mode flag is set, then both "quarter frame" and "half frame" signals are also generated
    when inhibit $ do
        setFrameInterruptFlag False
