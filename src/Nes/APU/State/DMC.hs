{-# LANGUAGE RecordWildCards #-}

module Nes.APU.State.DMC where

import Data.Array
import Data.Bits
import Data.List ((!?))
import Data.Maybe (fromMaybe, isNothing)
import Nes.Memory

data DMC = MkDMC
    { irqEnabledFlag :: Bool
    , loopFlag :: Bool
    , period :: Int
    , timer :: Int
    , sampleOgAddr :: Addr
    , sampleOgLength :: Int
    , sampleBufferAddr :: Addr -- Addr in memory of the sample buffer's byte
    , sampleBytesRemaining :: Int
    , sampleBuffer :: Maybe Byte
    , outputLevel :: Int
    , enableChannel :: Bool
    , shouldClock :: Bool
    , sleepingCycles :: Int
    , shiftRegister :: Byte
    , remainingBits :: Byte
    , silentFlag :: Bool
    }

newDMC :: DMC
newDMC = MkDMC{..}
  where
    irqEnabledFlag = False
    loopFlag = False
    period = 0
    timer = 0
    sampleOgAddr = 0
    sampleOgLength = 0
    sampleBufferAddr = 0
    sampleBytesRemaining = 0
    remainingBits = 0
    shiftRegister = 0
    silentFlag = False
    sampleBuffer = Nothing
    enableChannel = True
    outputLevel = 0
    shouldClock = False
    sleepingCycles = 0

getPeriodValue :: Int -> Int
getPeriodValue idx = fromMaybe 428 ([428, 380, 340, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54] !? idx)

clockDMC :: DMC -> DMC
clockDMC dmc =
    (if clocks then clockRemainingBits else id)
        dmc
            { timer = newTimer
            , outputLevel = newOutputLevel
            , shiftRegister = newShiftRegister
            }
  where
    clocks = timer dmc == 0
    newTimer = if timer dmc == 0 then period dmc else timer dmc - 1
    newShiftRegister = if clocks then shiftRegister dmc `shiftR` 1 else shiftRegister dmc
    newOutputLevel =
        if clocks && not (silentFlag dmc)
            then
                let delta = if shiftRegister dmc `testBit` 0 then 2 else (-2)
                    tmpOutLevel = outputLevel dmc + delta
                 in if (0, 127) `inRange` tmpOutLevel then tmpOutLevel else outputLevel dmc
            else outputLevel dmc

clockRemainingBits :: DMC -> DMC
clockRemainingBits dmc =
    dmc
        { remainingBits = newRemainingBits
        , silentFlag = newSilentFlag
        , shiftRegister = newShiftRegister
        , sampleBuffer = newSampleBuffer
        }
  where
    outputCycleEnds = remainingBits dmc == 1
    newRemainingBits = if remainingBits dmc == 1 then 8 else remainingBits dmc - 1
    newSilentFlag = outputCycleEnds && isNothing (sampleBuffer dmc)
    -- TODO Call loadSampleBuffer if we empty sample buffer
    (newShiftRegister, newSampleBuffer) = case (outputCycleEnds, sampleBuffer dmc) of
        (True, Just byte) -> (byte, Nothing)
        _ -> (shiftRegister dmc, sampleBuffer dmc)

reloadSample :: DMC -> DMC
reloadSample dmc =
    dmc
        { sampleBufferAddr = sampleOgAddr dmc
        , sampleBytesRemaining = sampleOgLength dmc
        , shouldClock = sampleOgLength dmc > 0
        }

-- | Loads the byte into the sample buffer and shift the sample buffer-related values
--
-- The first element of the returned tuple ays if the IRQ flag of the CPU should be set
loadSampleBuffer :: Byte -> DMC -> (Bool, DMC)
loadSampleBuffer byte dmc
    | sampleBytesRemaining dmc == 0 = (False, dmc)
    | otherwise = if shouldRestartSample then (False, reloadSample dmc1) else (shouldIRQ, dmc1)
  where
    dmc1 = dmc{sampleBuffer = Just byte, sampleBytesRemaining = newRemainingLength, sampleBufferAddr = newSampleAddr, shouldClock = newRemainingLength > 0}
    shouldRestartSample = newRemainingLength == 0 && loopFlag dmc
    shouldIRQ = newRemainingLength == 0 && irqEnabledFlag dmc
    newRemainingLength = sampleBytesRemaining dmc - 1
    newSampleAddr = let addr = sampleBufferAddr dmc + 1 in if addr >= 0xffff then addr - 0x8000 else addr

getDMCOutput :: DMC -> Int
getDMCOutput dmc = if silentFlag dmc then 0 else outputLevel dmc
