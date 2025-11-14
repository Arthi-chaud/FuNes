{-# LANGUAGE RecordWildCards #-}

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
) where

import Nes.APU.State.DMC
import Nes.APU.State.Filter.Chain
import Nes.APU.State.Filter.Constants (defaultOutputRate)
import Nes.APU.State.FrameCounter
import Nes.APU.State.Noise
import Nes.APU.State.Pulse
import Nes.APU.State.Triangle
import Nes.Bus.SideEffect (CPUSideEffect)
import Prelude hiding (cycle)

data APUState = MkAPUState
    { frameCounter :: !FrameCounter
    , pulse1 :: !Pulse
    , pulse2 :: !Pulse
    , triangle :: !Triangle
    , noise :: !Noise
    , dmc :: !DMC
    , cycle :: {-# UNPACK #-} !Int
    -- ^ Number of CPU cycles since the start
    , filterChain :: !FilterChain
    , sampleTimer :: {-# UNPACK #-} !Float
    -- ^ The number of CPU cycles since the last call to 'pushSampleCallback'
    , samplePeriod :: {-# UNPACK #-} !Float
    -- ^ The number of CPU cycles between each call to 'pushSampleCallback'
    , pushSampleCallback :: Float -> IO ()
    }

newAPUState :: (Float -> IO ()) -> APUState
newAPUState pushSampleCallback = MkAPUState{..}
  where
    frameCounter = newFrameCounter
    cycle = 0
    pulse1 = newPulse True
    pulse2 = newPulse False
    triangle = newTriangle
    noise = newNoise
    dmc = newDMC
    filterChain = newFilterChain defaultOutputRate
    samplePeriod = (21477272 / 12) / defaultOutputRate
    sampleTimer = samplePeriod

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
