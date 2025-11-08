{-# LANGUAGE RecordWildCards #-}

module Nes.APU.State.Noise (
    -- * Data type
    Noise (..),
    newNoise,
    getNoiseOutput,

    -- * Clock
    clockPulse,
    clockShiftRegister,

    -- * Utils
    getPeriodValue,
) where

import Data.Bits
import Data.List ((!?))
import Data.Maybe (fromMaybe)
import Data.Word
import Nes.APU.State.Envelope
import Nes.APU.State.LengthCounter

data Noise = MkN
    { useBit6ForFeedback :: Bool
    -- ^ AKA Mode flag
    , envelope :: Envelope
    , lengthCounter :: LengthCounter
    , shiftRegister :: Word16
    , period :: Int
    , timer :: Int
    }

newNoise :: Noise
newNoise = MkN{..}
  where
    envelope = newEnvelope
    useBit6ForFeedback = False
    shiftRegister = 1
    lengthCounter = newLengthCounter
    period = 0
    timer = 0

getPeriodValue :: Int -> Int
getPeriodValue idx = fromMaybe 4 ([4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068] !? idx)

instance HasLengthCounter Noise where
    getLengthCounter = lengthCounter
    setLengthCounter lc t = t{lengthCounter = lc}

instance HasEnvelope Noise where
    getEnvelope = envelope
    setEnvelope e t = t{envelope = e}

clockPulse :: Noise -> Noise
clockPulse n = clockCallback $ n{timer = newTimer}
  where
    newTimer = if timer n == 0 then period n else timer n - 1
    clockCallback = if timer n == 0 then clockShiftRegister else id

clockShiftRegister :: Noise -> Noise
clockShiftRegister n = n{shiftRegister = shift2}
  where
    shift0 = shiftRegister n
    xorBit = if useBit6ForFeedback n then 6 else 1
    feeback = (shift0 `testBit` 0) .^. (shift0 `testBit` xorBit)
    shift1 = shift0 `shiftR` 1
    shift2 = if feeback then shift1 `setBit` 14 else shift1

getNoiseOutput :: Noise -> Int
getNoiseOutput n = if shiftBit0IsSet || lengthCounterIsZero then 0 else getEnvelopeOutput $ envelope n
  where
    shiftBit0IsSet = shiftRegister n `testBit` 0
    lengthCounterIsZero = remainingLength (lengthCounter n) == 0
