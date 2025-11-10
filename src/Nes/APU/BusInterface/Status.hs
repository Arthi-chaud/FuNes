module Nes.APU.BusInterface.Status (write4015, read4015) where

import Control.Monad
import Data.Bits
import Nes.APU.Monad
import Nes.APU.State
import Nes.APU.State.DMC
import Nes.APU.State.FrameCounter
import Nes.APU.State.LengthCounter
import Nes.Memory

{-# INLINE write4015 #-}
write4015 :: Byte -> APU r ()
write4015 byte = do
    let enablePulse1Lc = byte `testBit` 0
        enablePulse2Lc = byte `testBit` 1
        enableTriangleLc = byte `testBit` 2
        enableNoiseLc = byte `testBit` 3
        enableDmc = byte `testBit` 4
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
    modifyAPUState $ modifyDMC $ \t ->
        if enableDmc
            -- TODO If there are bits remaining in the 1-byte sample buffer, these will finish playing before the next sample is fetched.
            then if sampleBytesRemaining t == 0 then restartSample t else t
            else t{sampleBytesRemaining = 0}

{-# INLINE read4015 #-}
read4015 :: APU r Byte
read4015 = do
    noiseBit <- withAPUState $ lengthCounterBit . noise
    triangleBit <- withAPUState $ lengthCounterBit . triangle
    pulse1Bit <- withAPUState $ lengthCounterBit . pulse1
    pulse2Bit <- withAPUState $ lengthCounterBit . pulse2
    dmcBit <- withAPUState $ \st -> sampleBytesRemaining (dmc st) > 0
    let frameInterruptBit = False -- TODO Should check if side effect if applied
    let dmcInterruptBit = False -- TODO Should check if side effect if applied
    -- TODO That Should be a CPUSideEffect
    modifyAPUState $ modifyFrameCounter $ \fc -> fc{frameInterruptFlag = False}
    -- TODO If an interrupt flag was set at the same moment of the read, it will read back as 1 but it will not be cleared.
    return $
        setBit' dmcInterruptBit 7 $
            setBit' frameInterruptBit 6 $
                setBit' dmcBit 4 $
                    setBit' noiseBit 3 $
                        setBit' triangleBit 2 $
                            setBit' pulse2Bit 1 $
                                setBit'
                                    pulse1Bit
                                    0
                                    0
  where
    setBit' b i a = if b then a `setBit` i else a `clearBit` i
    lengthCounterBit st = let lc = getLengthCounter st in remainingLength lc > 0 && not (isHalted lc)
