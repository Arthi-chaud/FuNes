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
) where

import Nes.APU.State.FrameCounter
import Nes.APU.State.Noise
import Nes.APU.State.Pulse
import Nes.APU.State.Triangle

data APUState = MkAPUState
    { frameCounter :: FrameCounter
    , pulse1 :: Pulse
    , pulse2 :: Pulse
    , triangle :: Triangle
    , noise :: Noise
    }

newAPUState :: APUState
newAPUState =
    MkAPUState newFrameCounter (newPulse True) (newPulse False) newTriangle newNoise

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

-- {-# INLINE modifyDMC #-}
-- modifyDMC :: (DMC -> DMC) -> APUState -> APUState
-- modifyDMC f st = st{dmc = f (dmc st)}
--

{-# INLINE modifyFrameCounter #-}
modifyFrameCounter :: (FrameCounter -> FrameCounter) -> APUState -> APUState
modifyFrameCounter f st = st{frameCounter = f (frameCounter st)}
