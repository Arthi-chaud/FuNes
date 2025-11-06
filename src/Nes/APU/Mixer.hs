module Nes.APU.Mixer (mix) where

import Nes.APU.Monad
import Nes.APU.State
import Nes.APU.State.BitField (BitField (get))
import Nes.APU.State.Common
import Nes.APU.State.Pulse
import Nes.Memory (Byte (unByte))

-- | Takes the channel outputs, converts them to an analog audio signal
--
-- Result is between 0 and 1
--
-- Source: https://www.nesdev.org/wiki/APU_Mixer
mix :: APU r Double
mix = do
    p1 <- getPulseOutput pulse1
    p2 <- getPulseOutput pulse2
    t <- getTriangleOutput
    n <- getNoiseOutput
    dmc <- getDMCOutput
    let pulseOut = 95.88 / (8128 / (p1 + p2)) + 100
        tndOut = 159.79 / (100 + (1 / (t / 8227) + (n / 12241) + (dmc / 22638)))
    return $ pulseOut + tndOut

getPulseOutput :: (APUState -> Pulse) -> APU r Double
getPulseOutput pulse = do
    volumeByte <- withAPUState $ get volume . pulse
    let output = fromIntegral $ unByte volumeByte
        isSilenced = False -- TODO https://www.nesdev.org/wiki/APU_Pulse#Pulse_channel_output_to_mixer
    return $ if isSilenced then 0 else output

getTriangleOutput :: APU r Double
getTriangleOutput = return 0 -- TODO

getNoiseOutput :: APU r Double
getNoiseOutput = return 0 -- TODO

getDMCOutput :: APU r Double
getDMCOutput = return 0 -- TODO
