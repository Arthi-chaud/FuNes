module Nes.APU.Mixer (runMixer) where

import Nes.APU.State
import Nes.APU.State.DMC (getDMCOutput)
import Nes.APU.State.Filter
import Nes.APU.State.Noise (getNoiseOutput)
import Nes.APU.State.Pulse (getPulseOutput)
import Nes.APU.State.Triangle (getTriangleOutput)

runMixer :: APUState -> (Float, APUState)
runMixer st =
    let
        pulse1Out = getPulseOutput . pulse1 $ st
        pulse2Out = getPulseOutput . pulse2 $ st
        triangleOut = getTriangleOutput . triangle $ st
        noiseOut = getNoiseOutput . noise $ st
        dmcOut = getDMCOutput . dmc $ st
        pulseOut = pulseTable (pulse1Out + pulse2Out)
        tndOut = tndTable (3 * triangleOut + 2 * noiseOut + dmcOut)
        mixerOutput = pulseOut + tndOut
        (res, newFilters) = processSample mixerOutput $ filterChain st
     in
        (res, st{filterChain = newFilters})

{-# INLINE pulseTable #-}
pulseTable :: Int -> Float
pulseTable 0 = 0
pulseTable n = 95.52 / ((8128.0 / fromIntegral n) + 100)

{-# INLINE tndTable #-}
tndTable :: Int -> Float
tndTable 0 = 0
tndTable n = 163.67 / ((24329.0 / fromIntegral n) + 100)
