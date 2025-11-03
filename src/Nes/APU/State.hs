module Nes.APU.State (
    -- * Definition
    APUState (..),
    newAPUState,

    -- * Setters
    modifyPulse1,
    modifyPulse2,
    modifyTriangle,
    modifyNoise,
    modifyDMC,
    modifyStatus,
    modifyFrameCounter,
) where

import Nes.APU.State.Channel (Channel (MkChannel), IsChannel (fromChannel))
import Nes.APU.State.DMC
import Nes.APU.State.FrameCounter
import Nes.APU.State.Noise
import Nes.APU.State.Pulse
import Nes.APU.State.StatusRegister
import Nes.APU.State.Triangle

data APUState = MkAPUState
    { pulse1 :: Pulse
    , pulse2 :: Pulse
    , triangle :: Triangle
    , noise :: Noise
    , dmc :: DMC
    , status :: StatusRegister
    , frameCounter :: FrameCounter
    }

newAPUState :: APUState
newAPUState =
    MkAPUState
        { pulse1 = mkChannel 0 0 0 0
        , pulse2 = mkChannel 0 0 0 0
        , triangle = mkChannel 0 0 0 0
        , noise = mkChannel 0 0 0 0
        , dmc = mkChannel 0 0 0 0
        , status = MkSR 0
        , frameCounter = MkFC 0
        }
  where
    mkChannel b1 b2 b3 b4 = fromChannel $ MkChannel b1 b2 b3 b4

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
modifyDMC f st = st{dmc = f (dmc st)}

{-# INLINE modifyStatus #-}
modifyStatus :: (StatusRegister -> StatusRegister) -> APUState -> APUState
modifyStatus f st = st{status = f (status st)}

{-# INLINE modifyFrameCounter #-}
modifyFrameCounter :: (FrameCounter -> FrameCounter) -> APUState -> APUState
modifyFrameCounter f st = st{frameCounter = f (frameCounter st)}
