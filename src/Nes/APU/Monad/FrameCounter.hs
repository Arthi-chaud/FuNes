module Nes.APU.Monad.FrameCounter (
    -- * Clocking
    clockFrameCounter,

    -- * Events
    runQuarterFrameEvent,
    runHalfFrameEvent,

    -- * statful setters
    setFrameInterruptFlag,
) where

import Control.Monad
import Nes.APU.Monad
import Nes.APU.State
import Nes.APU.State.Envelope (clockEnvelope, withEnvelope)
import Nes.APU.State.FrameCounter
import Nes.APU.State.Pulse (clockSweepUnit)

-- | Tells the frame counter to clock channels
--
-- Source: https://www.nesdev.org/wiki/APU_Frame_Counter
clockFrameCounter :: APU r ()
clockFrameCounter = do
    seqMode <- withAPUState $ sequenceMode . frameCounter
    case seqMode of
        FourStep -> clockFrameCounterFourStep
        FiveStep -> clockFrameCounterFiveStep
    modifyAPUState $ modifyFrameCounter incrementSequenceStep

clockFrameCounterFourStep :: APU r ()
clockFrameCounterFourStep = do
    step <- withAPUState $ sequenceStep . frameCounter
    inhibitFrameInterrupt <- withAPUState $ inhibitInterrupt . frameCounter

    when (step < 4) runHalfFrameEvent
    when (step == 1 || step == 3) runHalfFrameEvent
    when (step == 3 && not inhibitFrameInterrupt) $
        setFrameInterruptFlag True

clockFrameCounterFiveStep :: APU r ()
clockFrameCounterFiveStep = do
    step <- withAPUState $ sequenceStep . frameCounter
    when (step < 5) runQuarterFrameEvent
    when (step == 1 || step == 4) runHalfFrameEvent

runQuarterFrameEvent :: APU r ()
-- TODO clock all envelopes and triangle counter
runQuarterFrameEvent = do
    modifyAPUState $
        modifyPulse1 (withEnvelope clockEnvelope)
            . modifyPulse2 (withEnvelope clockEnvelope)

runHalfFrameEvent :: APU r ()
-- TODO clock all lengthcounters
runHalfFrameEvent = modifyAPUState $ \st ->
    st
        { pulse1 = clockSweepUnit (pulse1 st)
        , pulse2 = clockSweepUnit (pulse2 st)
        }

-- | Set the Frame Counter's Frame flag
setFrameInterruptFlag :: Bool -> APU r ()
setFrameInterruptFlag b = do
    -- TODO Connect to CPU 's IRQ
    modifyAPUState $
        modifyFrameCounter $
            \fc -> fc{frameInterruptFlag = b}
