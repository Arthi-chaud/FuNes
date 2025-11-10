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
    let seqMode = sequenceModeFromBool $ byte `testBit` 7
        inhibit = byte `testBit` 6
    modifyAPUState $
        modifyFrameCounter $
            \fc -> fc{sequenceMode = seqMode, inhibitInterrupt = inhibit}
    -- If the mode flag is set, then both "quarter frame" and "half frame" signals are also generated
    when (seqMode == FiveStep) $ do
        runQuarterFrameEvent
        runHalfFrameEvent
    when inhibit $ do
        setFrameInterruptFlag False
