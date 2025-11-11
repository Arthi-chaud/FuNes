module Nes.APU.State (
    -- * Definition
    APUState (..),
    newAPUState,

    -- * Setters
    modifyFrameCounter,
    modifyPulse1,
    modifyPulse2,
    modifyTriangle,
    modifyNoise,
    modifyDMC,
    modifyDMC',
    setCycleDeltaSinceLastSample,
    setSampleBufferSum,
) where

import Nes.APU.State.DMC
import Nes.APU.State.Filter
import Nes.APU.State.FrameCounter
import Nes.APU.State.Noise
import Nes.APU.State.Pulse
import Nes.APU.State.Triangle
import Nes.Bus.SideEffect (CPUSideEffect)

data APUState = MkAPUState
    { frameCounter :: !FrameCounter
    , pulse1 :: !Pulse
    , pulse2 :: !Pulse
    , triangle :: !Triangle
    , noise :: !Noise
    , dmc :: !DMC
    , filterChain :: !FilterChain
    , cycleDeltaSinceLastSample :: {-# UNPACK #-} !Int
    , samplesBufferSum :: {-# UNPACK #-} !Float
    , evenSampleCallbackCall :: {-# UNPACK #-} !Bool
    , pushSampleCallback :: !(Float -> IO ())
    }

newAPUState :: (Float -> IO ()) -> APUState
newAPUState =
    MkAPUState newFrameCounter (newPulse True) (newPulse False) newTriangle newNoise newDMC newFilterChain 0 0 True

{-# INLINE setCycleDeltaSinceLastSample #-}
setCycleDeltaSinceLastSample :: (Int -> Int) -> APUState -> APUState
setCycleDeltaSinceLastSample f fc =
    fc{cycleDeltaSinceLastSample = f $ cycleDeltaSinceLastSample fc}

{-# INLINE setSampleBufferSum #-}
setSampleBufferSum :: (Float -> Float) -> APUState -> APUState
setSampleBufferSum f fc =
    fc{samplesBufferSum = f $ samplesBufferSum fc}

{-# INLINE modifyPulse1 #-}
modifyPulse1 :: (Pulse -> Pulse) -> APUState -> APUState
modifyPulse1 f st = st{pulse1 = f (pulse1 st)}

{-# INLINE modifyPulse2 #-}
modifyPulse2 :: (Pulse -> Pulse) -> APUState -> APUState
modifyPulse2 f st = st{pulse2 = f (pulse2 st)}

{-# INLINE modifyTriangle #-}
modifyTriangle :: (Triangle -> Triangle) -> APUState -> APUState
modifyTriangle f st = st{triangle = f (triangle st)}

{-# INLINE modifyNoise #-}
modifyNoise :: (Noise -> Noise) -> APUState -> APUState
modifyNoise f st = st{noise = f (noise st)}

{-# INLINE modifyDMC #-}
modifyDMC :: (DMC -> DMC) -> APUState -> APUState
modifyDMC f st = let dmc' = f $ dmc st in st{dmc = dmc'}

{-# INLINE modifyDMC' #-}
modifyDMC' :: (DMC -> (DMC, CPUSideEffect)) -> APUState -> (APUState, CPUSideEffect)
modifyDMC' f st = let (dmc', sideEff) = f $ dmc st in (st{dmc = dmc'}, sideEff)

{-# INLINE modifyFrameCounter #-}
modifyFrameCounter :: (FrameCounter -> FrameCounter) -> APUState -> APUState
modifyFrameCounter f st = st{frameCounter = f (frameCounter st)}
