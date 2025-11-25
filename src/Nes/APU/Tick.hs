module Nes.APU.Tick (
    -- * Ticking
    -- $semantic
    tick,
    tickOnce,
    IsAPUCycle,

    -- * Internal ticking
    tickFrameCounter,
    runHalfFrameEvent,
    runQuarterFrameEvent,
    setFrameInterruptFlag,
) where

import Control.Monad
import Control.Monad.IO.Class
import Nes.APU.Mixer
import Nes.APU.Monad
import Nes.APU.State
import Nes.APU.State.DMC
import Nes.APU.State.Envelope
import Nes.APU.State.Filter.Thread
import Nes.APU.State.FrameCounter
import qualified Nes.APU.State.FrameCounter as FC
import Nes.APU.State.LengthCounter
import Nes.APU.State.Noise
import Nes.APU.State.Pulse
import Nes.APU.State.Triangle
import Nes.Internal.MonadState
import Nes.Interrupt
import Prelude hiding (cycle)

-- $semantic
--     The APU being a part of the CPU, they both tick at the same time. However, some ticks are updated every other CPU cycles.
--     Here the 'tick' function should be called every CPU tick, and pass as parameter whether the tick is on an even CPU cycle or not.
--     Same goes for 'tickMany'.

type IsAPUCycle = Bool

-- | Calls 'tick' n amount of time
--
-- the first parameter says whether the first tick is an APU cycle or not
tick :: IsAPUCycle -> Int -> APU r ()
tick _ 0 = return ()
tick b n = tickOnce b >> tick (not b) (n - 1)

{-# INLINE tickOnce #-}
tickOnce :: IsAPUCycle -> APU r ()
tickOnce isAPUCycle = do
    -- Ticks
    tickDelayedWriteBuffer
    modifyDMCAndInterrupt tickDMC
    triangle %= tickTriangle
    noise %= tickNoise
    when isAPUCycle $ do
        pulse1 %= tickPulse
        pulse2 %= tickPulse
        tickFrameCounter
    -- Mixing
    sample <- gets getMixerOutput
    liftIO . (`consumeSample` sample) =<< use filterThread
    sampleTimer += (-1)
    sampleTimer' <- use sampleTimer
    when (sampleTimer' <= 1) $ do
        !filterOut <- liftIO . outputSample =<< use filterThread
        !callback <- use pushSampleCallback
        liftIO $ callback filterOut
        (sampleTimer +=) =<< use samplePeriod
    cycle += 1

-- | Tells the frame counter to tick channels
--
-- Source: https://www.nesdev.org/wiki/APU_Frame_Counter
tickFrameCounter :: APU r ()
tickFrameCounter = do
    reset <- uses frameCounter shouldResetSequenceStep
    seqMode <- uses frameCounter sequenceMode
    if reset
        then resetFrameCounterSequence
        else do
            fc <- use frameCounter
            when (shouldIncrementSequenceStep fc) $ do
                case seqMode of
                    FourStep -> tickFrameCounterFourStep
                    FiveStep -> tickFrameCounterFiveStep
                frameCounter %= incrementSequenceStep
            frameCounter %= \f -> f{cycles = 1 + cycles f}

tickDelayedWriteBuffer :: APU r ()
tickDelayedWriteBuffer = do
    fc <- use frameCounter
    case delayedWriteSideEffectCycle fc of
        Nothing -> return ()
        Just 0 ->
            frameCounter
                %= const fc{delayedWriteSideEffectCycle = Nothing, FC.sequenceStep = 0, cycles = 0}
        Just n ->
            frameCounter
                %= const fc{delayedWriteSideEffectCycle = Just $ n - 1}

resetFrameCounterSequence :: APU r ()
resetFrameCounterSequence = do
    frameCounter %= resetSequence
    seqMode <- uses frameCounter sequenceMode
    inhibitFrameInterrupt <- uses frameCounter inhibitInterrupt
    when (seqMode == FourStep && not inhibitFrameInterrupt) $ do
        setFrameInterruptFlag True

tickFrameCounterFourStep :: APU r ()
tickFrameCounterFourStep = do
    step <- uses frameCounter FC.sequenceStep
    inhibitFrameInterrupt <- uses frameCounter inhibitInterrupt
    when (step < 4) runQuarterFrameEvent
    when (step == 1 || step == 3) runHalfFrameEvent
    when (step == 4) $ -- Flag should be cleared when going from put to get
        setFrameInterruptFlag False
    when (step == 3 && not inhibitFrameInterrupt) $
        setFrameInterruptFlag True

tickFrameCounterFiveStep :: APU r ()
tickFrameCounterFiveStep = do
    step <- uses frameCounter FC.sequenceStep
    when (step < 5 && step /= 3) runQuarterFrameEvent
    when (step == 1 || step == 4) runHalfFrameEvent

runQuarterFrameEvent :: APU r ()
runQuarterFrameEvent = do
    pulse1 %= withEnvelope tickEnvelope
    pulse2 %= withEnvelope tickEnvelope
    noise %= withEnvelope tickEnvelope
    triangle %= tickTriangleLinearCounter

runHalfFrameEvent :: APU r ()
runHalfFrameEvent = do
    pulse1 %= (withLengthCounter tickLengthCounter . tickSweepUnit)
    pulse2 %= (withLengthCounter tickLengthCounter . tickSweepUnit)
    noise %= withLengthCounter tickLengthCounter
    triangle %= withLengthCounter tickLengthCounter

-- | Set the Frame Counter's Frame flag
{-# INLINE setFrameInterruptFlag #-}
setFrameInterruptFlag :: Bool -> APU r ()
setFrameInterruptFlag b = do
    modify $ \s -> s{irq = Just FrameCounter}
    frameCounter %= \fc -> fc{frameInterruptFlag = b}
