{-# LANGUAGE RecordWildCards #-}

module Nes.APU.Mixer (getMixerOutput) where

import Nes.APU.State
import Nes.APU.State.DMC (getDMCOutput)
import Nes.APU.State.Noise (getNoiseOutput)
import Nes.APU.State.Pulse (getPulseOutput)
import Nes.APU.State.Triangle (getTriangleOutput)
import Prelude hiding (cycle)

getMixerOutput :: APUState -> Float
getMixerOutput MkAPUState{..} =
    let
        !pulse1Out = getPulseOutput pulse1
        !pulse2Out = getPulseOutput pulse2
        !triangleOut = getTriangleOutput triangle
        !noiseOut = getNoiseOutput noise
        !dmcOut = getDMCOutput dmc
        !pulseOut = pulseTable (pulse1Out + pulse2Out)
        !tndOut = tndTable (3 * triangleOut + 2 * noiseOut + dmcOut)
        !mixerOutput = pulseOut + tndOut
     in
        mixerOutput

{-# INLINE pulseTable #-}
pulseTable :: Int -> Float
pulseTable 0 = 0
pulseTable n = 95.52 / ((8128.0 / fromIntegral n) + 100)

{-# INLINE tndTable #-}
tndTable :: Int -> Float
tndTable 0 = 0
tndTable n = 163.67 / ((24329.0 / fromIntegral n) + 100)
