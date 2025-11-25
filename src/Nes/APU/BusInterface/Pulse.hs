{-# LANGUAGE RankNTypes #-}

module Nes.APU.BusInterface.Pulse (
    -- * Pulse 1
    write4000,
    write4001,
    write4002,
    write4003,

    -- * Pulse 2
    write4004,
    write4005,
    write4006,
    write4007,
) where

import Control.Lens (Lens')
import Data.Bits
import Nes.APU.Monad
import Nes.APU.State
import Nes.APU.State.Envelope
import Nes.APU.State.LengthCounter
import Nes.APU.State.Pulse
import Nes.Internal.MonadState
import Nes.Memory

{-# INLINE write4000 #-}
write4000 :: Byte -> APU r ()
write4000 = writePulseFirstByte pulse1

{-# INLINE write4004 #-}
write4004 :: Byte -> APU r ()
write4004 = writePulseFirstByte pulse2

{-# INLINE writePulseFirstByte #-}
writePulseFirstByte :: Lens' APUState Pulse -> Byte -> APU r ()
writePulseFirstByte l byte = do
    let duty = byte `shiftR` 6
        haltLC = byte `testBit` 5
        constVol = byte `testBit` 4
        vol = byte .&. 0b1111
    l
        %= withEnvelope
            ( \e ->
                e
                    { constantVolume = byteToInt vol
                    , useConstantVolume = constVol
                    , loopFlag = haltLC
                    }
            )
    l %= withLengthCounter (\lc -> lc{isHalted = haltLC})
    l %= \p -> p{dutyIndex = fromIntegral $ unByte duty}

{-# INLINE write4001 #-}
write4001 :: Byte -> APU r ()
write4001 = writePulseSecondByte pulse1

{-# INLINE write4005 #-}
write4005 :: Byte -> APU r ()
write4005 = writePulseSecondByte pulse2

{-# INLINE writePulseSecondByte #-}
writePulseSecondByte :: Lens' APUState Pulse -> Byte -> APU r ()
writePulseSecondByte l byte = do
    let enabledFlag = byte `testBit` 7
        divPeriod = 1 + ((byte `shiftR` 4) .&. 0b111)
        negateFlag = byte `testBit` 3
        shiftC = byte .&. 0b111
        sweepIsEnabled = enabledFlag && shiftC > 0
    l
        %= modifySweep
            ( \s ->
                s
                    { reloadFlag = True
                    , enabled = sweepIsEnabled
                    , dividerPeriod = byteToInt divPeriod
                    , negateDelta = negateFlag
                    , shiftCount = byteToInt shiftC
                    }
            )
    l %= updateTargetPeriod

{-# INLINE write4002 #-}
write4002 :: Byte -> APU r ()
write4002 = writePulseThirdByte pulse1

{-# INLINE write4006 #-}
write4006 :: Byte -> APU r ()
write4006 = writePulseThirdByte pulse2

{-# INLINE writePulseThirdByte #-}
writePulseThirdByte :: Lens' APUState Pulse -> Byte -> APU r ()
writePulseThirdByte l byte =
    l %= \p ->
        let newPeriod = (period p .&. 0b11100000000) .|. byteToInt byte
         in updateTargetPeriod $ p{period = newPeriod}

{-# INLINE write4003 #-}
write4003 :: Byte -> APU r ()
write4003 = writePulseFourthByte pulse1

{-# INLINE write4007 #-}
write4007 :: Byte -> APU r ()
write4007 = writePulseFourthByte pulse2

{-# INLINE writePulseFourthByte #-}
writePulseFourthByte :: Lens' APUState Pulse -> Byte -> APU r ()
writePulseFourthByte l byte =
    l %= \p ->
        let newPeriod = ((byteToInt byte .&. 0b111) `shiftL` 8) .|. (period p .&. 0b11111111)
            newLCLoad = byteToInt byte `shiftR` 3
         in updateTargetPeriod $
                withEnvelope (\e -> e{startFlag = True}) $
                    withLengthCounter
                        (loadLengthCounter newLCLoad)
                        p
                            { period = newPeriod
                            , dutyStep = 0
                            }
