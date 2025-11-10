{-# LANGUAGE RecordWildCards #-}

module Nes.APU.State.DMC (
    DMC (..),
    newDMC,
    tickDMC,
    getPeriodValue,

    -- * Actions
    restartSample,
    loadSampleBuffer,

    -- * Output
    getDMCOutput,
) where

import Data.Array
import Data.Bits
import Data.List ((!?))
import Data.Maybe (fromMaybe, isNothing)
import Nes.Bus.SideEffect (CPUSideEffect (setIRQ, startDMCDMA))
import Nes.Memory

data DMC = MkDMC
    { irqEnabledFlag :: {-# UNPACK #-} !Bool
    , loopFlag :: {-# UNPACK #-} !Bool
    , period :: {-# UNPACK #-} !Int
    , timer :: {-# UNPACK #-} !Int
    , sampleOgAddr :: {-# UNPACK #-} !Addr
    , sampleOgLength :: {-# UNPACK #-} !Int
    , sampleBufferAddr :: {-# UNPACK #-} !Addr -- Addr in memory of the sample buffer's byte
    , sampleBytesRemaining :: {-# UNPACK #-} !Int
    , sampleBuffer :: {-# UNPACK #-} !(Maybe Byte)
    , outputLevel :: {-# UNPACK #-} !Int
    , enableChannel :: {-# UNPACK #-} !Bool
    , shouldClock :: {-# UNPACK #-} !Bool
    , sleepingCycles :: {-# UNPACK #-} !Int
    , shiftRegister :: {-# UNPACK #-} !Byte
    , remainingBits :: {-# UNPACK #-} !Byte
    , silentFlag :: {-# UNPACK #-} !Bool
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

-- | When a sample is (re)started, the current address is set to the sample address, and bytes remaining is set to the sample length.
restartSample :: DMC -> DMC
restartSample dmc =
    dmc
        { sampleBufferAddr = sampleOgAddr dmc
        , sampleBytesRemaining = sampleOgLength dmc
        , shouldClock = sampleOgLength dmc > 0
        }

getDMCOutput :: DMC -> Int
getDMCOutput dmc = if silentFlag dmc then 0 else outputLevel dmc

tickDMC :: DMC -> (DMC, CPUSideEffect)
tickDMC dmc =
    (if clocks then tickOutputUnit else (,mempty))
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

tickOutputUnit :: DMC -> (DMC, CPUSideEffect)
tickOutputUnit dmc = if isEndOfOutputCycle then onOutputCycleEnd dmc1 else (dmc1, mempty)
  where
    newRemainingBits = max 0 (remainingBits dmc - 1)
    isEndOfOutputCycle = newRemainingBits == 0
    dmc1 = dmc{remainingBits = newRemainingBits}

onOutputCycleEnd :: DMC -> (DMC, CPUSideEffect)
onOutputCycleEnd dmc = (dmc1, sideEffect)
  where
    dmc0 = dmc{remainingBits = 8}
    dmc1 = case sampleBuffer dmc0 of
        Nothing -> dmc0{silentFlag = True}
        Just b -> dmc0{shiftRegister = b, sampleBuffer = Nothing}
    sideEffect = mempty{startDMCDMA = isNothing (sampleBuffer dmc1) && sampleBytesRemaining dmc1 > 0}

-- | Loads the byte into the sample buffer and shift the sample buffer-related values
loadSampleBuffer :: Byte -> DMC -> (DMC, CPUSideEffect)
loadSampleBuffer byte dmc =
    let
        newSampleBufferAddr = let addr = sampleBufferAddr dmc + 1 in if addr >= 0xffff then addr - 0x8000 else addr
        newRemainingLength = max 0 (sampleBytesRemaining dmc - 1)
        dmc1 =
            dmc
                { sampleBuffer = Just byte
                , sampleBytesRemaining = newRemainingLength
                , sampleBufferAddr = newSampleBufferAddr
                , shouldClock = newRemainingLength > 0
                }
        shouldRestartSample = newRemainingLength == 0 && loopFlag dmc
        shouldIRQ = newRemainingLength == 0 && irqEnabledFlag dmc
     in
        if shouldRestartSample
            then (restartSample dmc1, mempty)
            else (dmc1, mempty{setIRQ = shouldIRQ})
