module Nes.APU.BusInterface (write4017) where

import Control.Monad
import Data.Bits
import Nes.APU.Monad
import Nes.APU.Monad.FrameCounter
import Nes.APU.State
import Nes.APU.State.FrameCounter
import Nes.Memory (Byte)

-- | Callback when a byte is written to 0x4017 through the Bus
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

write4015 :: Byte -> APU r ()
write4015 byte = do
    let enablePulse1Lc = byte `testBit` 0
        enablePulse2Lc = byte `testBit` 1
        enableTriangeLc = byte `testBit` 2
        enableNoiseLc = byte `testBit` 3
        enableDmc = byte `testBit` 4
    -- TODO: For each LC: If enable is false, call 'clearRemainingLength'
    return ()
