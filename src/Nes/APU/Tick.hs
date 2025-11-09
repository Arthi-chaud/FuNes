module Nes.APU.Tick (
    -- * Ticking
    -- $semantic
    tick,
    tickOnce,
    IsAPUCycle,

    -- * Internal clocking
    clockFrameCounter,
    runHalfFrameEvent,
    runQuarterFrameEvent,
    setFrameInterruptFlag,
) where

import Control.Monad
import Nes.APU.Monad
import Nes.APU.State
import Nes.APU.State.DMC
import Nes.APU.State.Envelope
import Nes.APU.State.FrameCounter
import qualified Nes.APU.State.FrameCounter as FC
import Nes.APU.State.LengthCounter
import Nes.APU.State.Pulse
import Nes.APU.State.Triangle

-- $use
--     The APU being a part of the CPU, they both tick at the same time. However, some clocks are updated every other CPU cycles.
--     Here the 'tick' function should be called every CPU tick, and pass as parameter whether the tick is on an even CPU cycle or not.
--     Same goes for 'tickMany'.

type IsAPUCycle = Bool

-- | Calls 'tick' n amount of time
--
-- the first parameter says whether the first tick is an APU cycle or not
tick :: IsAPUCycle -> Int -> APU r ()
tick _ 0 = return ()
tick b n = tickOnce b >> tick (not b) (n - 1)

tickOnce :: IsAPUCycle -> APU r ()
tickOnce isAPUCycle = do
    modifyAPUState $ modifyDMC clockDMC
    modifyAPUState $ modifyTriangle clockTriangle
    when isAPUCycle $ do
        modifyAPUState $
            modifyPulse1 clockPulse
                . modifyPulse2 clockPulse
        clockFrameCounter

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
    step <- withAPUState $ FC.sequenceStep . frameCounter
    inhibitFrameInterrupt <- withAPUState $ inhibitInterrupt . frameCounter

    when (step < 4) runHalfFrameEvent
    when (step == 1 || step == 3) runHalfFrameEvent
    when (step == 3 && not inhibitFrameInterrupt) $
        setFrameInterruptFlag True

clockFrameCounterFiveStep :: APU r ()
clockFrameCounterFiveStep = do
    step <- withAPUState $ FC.sequenceStep . frameCounter
    when (step < 5) runQuarterFrameEvent
    when (step == 1 || step == 4) runHalfFrameEvent

runQuarterFrameEvent :: APU r ()
runQuarterFrameEvent = do
    modifyAPUState $
        modifyPulse1 (withEnvelope clockEnvelope)
            . modifyPulse2 (withEnvelope clockEnvelope)
            . modifyNoise (withEnvelope clockEnvelope)
            . modifyTriangle clockTriangleLinearCounter

runHalfFrameEvent :: APU r ()
-- TODO clock all lengthcounters
runHalfFrameEvent = modifyAPUState $ \st ->
    st
        { pulse1 = withLengthCounter clockLengthCounter $ clockSweepUnit (pulse1 st)
        , pulse2 = withLengthCounter clockLengthCounter $ clockSweepUnit (pulse2 st)
        , triangle = withLengthCounter clockLengthCounter $ triangle st
        , noise = withLengthCounter clockLengthCounter $ noise st
        }

-- | Set the Frame Counter's Frame flag
setFrameInterruptFlag :: Bool -> APU r ()
setFrameInterruptFlag b = do
    -- TODO Connect to CPU 's IRQ
    modifyAPUState $
        modifyFrameCounter $
            \fc -> fc{frameInterruptFlag = b}
