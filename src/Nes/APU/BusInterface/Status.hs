module Nes.APU.BusInterface.Status (write4015) where

import Control.Monad
import Data.Bits
import Nes.APU.Monad
import Nes.APU.State
import Nes.APU.State.LengthCounter
import Nes.Memory

write4015 :: Byte -> APU r ()
write4015 byte = do
    let enablePulse1Lc = byte `testBit` 0
        enablePulse2Lc = byte `testBit` 1
        enableTriangleLc = byte `testBit` 2
        enableNoiseLc = byte `testBit` 3
        enableDmc = byte `testBit` 4
    -- TODO: For each LC: If enable is false, call 'clearRemainingLength'
    -- TODO: Handle DMC side effects
    unless enablePulse1Lc $
        modifyAPUState $
            modifyPulse1 $
                withLengthCounter clearAndHaltLengthCounter

    unless enablePulse2Lc $
        modifyAPUState $
            modifyPulse2 $
                withLengthCounter clearAndHaltLengthCounter

    unless enableTriangleLc $
        modifyAPUState $
            modifyTriangle $
                withLengthCounter clearAndHaltLengthCounter

    unless enableNoiseLc $
        modifyAPUState $
            modifyNoise $
                withLengthCounter clearAndHaltLengthCounter
