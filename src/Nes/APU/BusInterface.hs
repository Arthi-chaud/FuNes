module Nes.APU.BusInterface (
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

    -- * Status register
    write4015,

    -- * Frame counter
    write4017,
) where

import Control.Monad
import Data.Bits
import Nes.APU.Monad
import Nes.APU.Monad.FrameCounter
import Nes.APU.State
import Nes.APU.State.Envelope (Envelope (constantVolume, loopFlag, useConstantVolume), withEnvelope)
import Nes.APU.State.FrameCounter
import Nes.APU.State.LengthCounter
import Nes.APU.State.Pulse
import Nes.Memory (Byte (..), byteToInt)

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
    unless enablePulse1Lc $
        modifyAPUState $
            modifyPulse1 $
                withLengthCounter clearLengthCounter

    unless enablePulse2Lc $
        modifyAPUState $
            modifyPulse2 $
                withLengthCounter clearLengthCounter

write4000 :: Byte -> APU r ()
write4000 = writePulseFirstByte modifyPulse1

write4004 :: Byte -> APU r ()
write4004 = writePulseFirstByte modifyPulse2

{-# INLINE writePulseFirstByte #-}
writePulseFirstByte :: ((Pulse -> Pulse) -> APUState -> APUState) -> Byte -> APU r ()
writePulseFirstByte setter byte = do
    let duty = byte `shiftR` 6
        haltLC = byte `testBit` 5
        constVol = byte `testBit` 4
        vol = byte .&. 0b1111
    modifyAPUState $ setter $ \p ->
        withEnvelope (\e -> e{constantVolume = fromIntegral $ unByte vol, useConstantVolume = constVol, loopFlag = haltLC}) $
            withLengthCounter (\lc -> lc{isHalted = haltLC}) $
                p{dutyIndex = fromIntegral $ unByte duty}

write4001 :: Byte -> APU r ()
write4001 = writePulseSecondByte modifyPulse1

write4005 :: Byte -> APU r ()
write4005 = writePulseSecondByte modifyPulse2

{-# INLINE writePulseSecondByte #-}
writePulseSecondByte :: ((Pulse -> Pulse) -> APUState -> APUState) -> Byte -> APU r ()
writePulseSecondByte setter byte = do
    let enabledFlag = byte `testBit` 7
        divPeriod = (byte `shiftR` 4) .&. 0b111
        negateFlag = byte `testBit` 3
        shiftC = byte .&. 0b111
        sweepIsEnabled = enabledFlag && shiftC > 0
    modifyAPUState $
        setter $
            updateTargetPeriod
                . modifySweep
                    ( \s ->
                        s
                            { reloadFlag = True
                            , enabled = sweepIsEnabled
                            , dividerPeriod = byteToInt divPeriod
                            , negateDelta = negateFlag
                            , shiftCount = byteToInt shiftC
                            }
                    )

write4002 :: Byte -> APU r ()
write4002 = writePulseThirdByte modifyPulse1

write4006 :: Byte -> APU r ()
write4006 = writePulseThirdByte modifyPulse2

{-# INLINE writePulseThirdByte #-}
writePulseThirdByte :: ((Pulse -> Pulse) -> APUState -> APUState) -> Byte -> APU r ()
writePulseThirdByte setter byte = modifyAPUState $ setter $ \p ->
    let newPeriod = (period p .&. 0b11100000000) .|. byteToInt byte
     in updateTargetPeriod $ p{period = newPeriod}

write4003 :: Byte -> APU r ()
write4003 = writePulseFourthByte modifyPulse1

write4007 :: Byte -> APU r ()
write4007 = writePulseFourthByte modifyPulse2

{-# INLINE writePulseFourthByte #-}
writePulseFourthByte :: ((Pulse -> Pulse) -> APUState -> APUState) -> Byte -> APU r ()
writePulseFourthByte setter byte = modifyAPUState $ setter $ \p ->
    let newPeriod = ((byteToInt byte .&. 0b111) `shiftL` 8) .|. (period p .&. 0b11111111)
        newLCLoad = byteToInt byte `shiftR` 3
     in updateTargetPeriod $
            withLengthCounter
                (loadLengthCounter newLCLoad)
                p
                    { period = newPeriod
                    , dutyStep = 0
                    -- TODO Not sure
                    -- https://www.nesdev.org/wiki/APU_Pulse#Registers
                    }
