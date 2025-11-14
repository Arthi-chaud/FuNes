module Nes.APU.BusInterface.Status (write4015, read4015) where

import Control.Monad
import Data.Bits
import Nes.APU.Monad
import Nes.APU.State
import Nes.APU.State.DMC
import Nes.APU.State.LengthCounter
import Nes.APU.Tick (setFrameInterruptFlag)
import Nes.Bus.SideEffect
import Nes.FlagRegister (getFlag)
import Nes.Memory

{-# INLINE write4015 #-}
write4015 :: Byte -> APU r ()
write4015 byte = do
    let enablePulse1Lc = byte `testBit` 0
        enablePulse2Lc = byte `testBit` 1
        enableTriangleLc = byte `testBit` 2
        enableNoiseLc = byte `testBit` 3
        enableDmc = byte `testBit` 4
    toggleLengthCounter enablePulse1Lc modifyPulse1
    toggleLengthCounter enablePulse2Lc modifyPulse2
    toggleLengthCounter enableTriangleLc modifyTriangle
    toggleLengthCounter enableNoiseLc modifyNoise
    modifyAPUState $ modifyDMC $ \t ->
        if enableDmc
            -- TODO If there are bits remaining in the 1-byte sample buffer, these will finish playing before the next sample is fetched.
            then if sampleBytesRemaining t == 0 then restartSample t else t
            else t{sampleBytesRemaining = 0}

{-# INLINE toggleLengthCounter #-}
toggleLengthCounter :: (HasLengthCounter a) => Bool -> ((a -> a) -> APUState -> APUState) -> APU r ()
toggleLengthCounter enable f =
    modifyAPUState $
        f $
            withLengthCounter $
                if enable then enableLengthCounter else disableLengthCounter . clearAndHaltLengthCounter

{-# INLINE read4015 #-}
read4015 :: APU r Byte
read4015 = do
    noiseBit <- withAPUState $ lengthCounterBit . noise
    triangleBit <- withAPUState $ lengthCounterBit . triangle
    pulse1Bit <- withAPUState $ lengthCounterBit . pulse1
    pulse2Bit <- withAPUState $ lengthCounterBit . pulse2
    dmcBit <- withAPUState $ \st -> sampleBytesRemaining (dmc st) > 0
    frameInterruptBit <- withSideEffect $ getFlag IRQ
    dmcInterruptBit <- withSideEffect $ getFlag DMCDMA
    when frameInterruptBit $ do
        setFrameInterruptFlag False
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
    lengthCounterBit st = let lc = getLengthCounter st in isEnabled lc
