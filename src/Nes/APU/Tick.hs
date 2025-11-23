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
import qualified Nes.APU.State as S
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
    modify $ uncurry (modifyDMC' tickDMC)
    modify $
        modifyTriangle tickTriangle
            . modifyNoise tickNoise
    when isAPUCycle $ do
        modify $
            modifyPulse1 tickPulse
                . modifyPulse2 tickPulse
        tickFrameCounter
    -- Mixing
    sample <- gets getMixerOutput
    liftIO . (`consumeSample` sample) =<< gets filterThread
    modify $ \st -> st{sampleTimer = sampleTimer st - 1}
    sampleTimer' <- gets sampleTimer
    when (sampleTimer' <= 1) $ do
        !filterOut <- liftIO . outputSample =<< gets filterThread
        !callback <- gets pushSampleCallback
        liftIO $ callback filterOut
        modify $
            \st -> st{sampleTimer = S.sampleTimer st + S.samplePeriod st}
    modify $ \st -> st{cycle = cycle st + 1}

-- | Tells the frame counter to tick channels
--
-- Source: https://www.nesdev.org/wiki/APU_Frame_Counter
tickFrameCounter :: APU r ()
tickFrameCounter = do
    reset <- gets $ shouldResetSequenceStep . frameCounter
    seqMode <- gets $ sequenceMode . frameCounter
    if reset
        then resetFrameCounterSequence
        else do
            fc <- gets frameCounter
            when (shouldIncrementSequenceStep fc) $ do
                case seqMode of
                    FourStep -> tickFrameCounterFourStep
                    FiveStep -> tickFrameCounterFiveStep
                modify $ modifyFrameCounter incrementSequenceStep
            modify $ modifyFrameCounter $ setCycles (+ 1)

tickDelayedWriteBuffer :: APU r ()
tickDelayedWriteBuffer = do
    fc <- gets frameCounter
    case delayedWriteSideEffectCycle fc of
        Nothing -> return ()
        Just 0 ->
            modify $
                modifyFrameCounter $
                    const fc{delayedWriteSideEffectCycle = Nothing, FC.sequenceStep = 0, cycles = 0}
        Just n ->
            modify $
                modifyFrameCounter $
                    const fc{delayedWriteSideEffectCycle = Just $ n - 1}

resetFrameCounterSequence :: APU r ()
resetFrameCounterSequence = do
    modify $ modifyFrameCounter resetSequence
    seqMode <- gets $ sequenceMode . frameCounter
    inhibitFrameInterrupt <- gets $ inhibitInterrupt . frameCounter
    when (seqMode == FourStep && not inhibitFrameInterrupt) $ do
        setFrameInterruptFlag True

tickFrameCounterFourStep :: APU r ()
tickFrameCounterFourStep = do
    step <- gets $ FC.sequenceStep . frameCounter
    inhibitFrameInterrupt <- gets $ inhibitInterrupt . frameCounter
    when (step < 4) runQuarterFrameEvent
    when (step == 1 || step == 3) runHalfFrameEvent
    when (step == 4) $ -- Flag should be cleared when going from put to get
        setFrameInterruptFlag False
    when (step == 3 && not inhibitFrameInterrupt) $
        setFrameInterruptFlag True

tickFrameCounterFiveStep :: APU r ()
tickFrameCounterFiveStep = do
    step <- gets $ FC.sequenceStep . frameCounter
    when (step < 5 && step /= 3) runQuarterFrameEvent
    when (step == 1 || step == 4) runHalfFrameEvent

runQuarterFrameEvent :: APU r ()
runQuarterFrameEvent = do
    modify $
        modifyPulse1 (withEnvelope tickEnvelope)
            . modifyPulse2 (withEnvelope tickEnvelope)
            . modifyNoise (withEnvelope tickEnvelope)
            . modifyTriangle tickTriangleLinearCounter

runHalfFrameEvent :: APU r ()
runHalfFrameEvent = modify $ \st ->
    st
        { pulse1 = withLengthCounter tickLengthCounter $ tickSweepUnit (pulse1 st)
        , pulse2 = withLengthCounter tickLengthCounter $ tickSweepUnit (pulse2 st)
        , triangle = withLengthCounter tickLengthCounter $ triangle st
        , noise = withLengthCounter tickLengthCounter $ noise st
        }

-- | Set the Frame Counter's Frame flag
{-# INLINE setFrameInterruptFlag #-}
setFrameInterruptFlag :: Bool -> APU r ()
setFrameInterruptFlag b = do
    modify $ \s -> s{irq = Just FrameCounter}
    modify $
        modifyFrameCounter $
            \fc -> fc{frameInterruptFlag = b}
