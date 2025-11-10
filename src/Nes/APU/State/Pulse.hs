{-# LANGUAGE RecordWildCards #-}

module Nes.APU.State.Pulse (
    -- * Pulse
    Pulse (..),
    newPulse,
    tickPulse,
    modifySweep,
    withSweep,

    -- * Sweep Unit
    SweepUnit (..),
    tickSweepUnit,
    updateTargetPeriod,

    -- * Output
    getPulseOutput,
) where

import Data.Bits
import Data.List ((!?))
import Data.Maybe (fromMaybe)
import Nes.APU.State.Envelope
import Nes.APU.State.LengthCounter

data Pulse = MkP
    { dutyIndex :: {-# UNPACK #-} !Int
    -- ^ Index for the 'dutySequences' table
    , dutyStep :: {-# UNPACK #-} !Int
    -- ^ Index for a row's element in the 'dutySequences' table
    , lengthCounter :: !LengthCounter
    , period :: {-# UNPACK #-} !Int
    -- ^ Max value of the timer
    , timer :: {-# UNPACK #-} !Int
    -- ^ Decreases each tick, from 'period' to 0 and loops
    , sweepUnit :: !SweepUnit
    , envelope :: {-# UNPACK #-} !Envelope
    }

-- | Args is true if building pulse 1
newPulse :: Bool -> Pulse
newPulse isPulseOne = MkP{..}
  where
    dutyIndex = 0
    dutyStep = 0
    lengthCounter = newLengthCounter
    period = 0
    timer = 0
    sweepUnit = MkSU False 0 0 False 0 0 False isPulseOne
    envelope = newEnvelope

--

data SweepUnit = MkSU
    { enabled :: Bool
    , dividerPeriod :: Int
    , dividerCounter :: Int
    , negateDelta :: Bool
    , targetPeriod :: Int
    , shiftCount :: Int
    , reloadFlag :: Bool
    , isPulse1 :: Bool
    }

{-# INLINE modifySweep #-}
modifySweep :: (SweepUnit -> SweepUnit) -> Pulse -> Pulse
modifySweep f p = p{sweepUnit = f (sweepUnit p)}

{-# INLINE withSweep #-}
withSweep :: (SweepUnit -> a) -> Pulse -> a
withSweep f p = f (sweepUnit p)

-- | Update the target period in the Sweep unit of the pulse
updateTargetPeriod :: Pulse -> Pulse
updateTargetPeriod p =
    modifySweep
        ( \s ->
            let
                delta = period p `shiftR` shiftCount s
             in
                s
                    { targetPeriod =
                        if negateDelta s
                            then period p - delta - fromEnum (isPulse1 s)
                            else period p + delta
                    }
        )
        p

tickPulse :: Pulse -> Pulse
tickPulse p = p{dutyStep = newDutyStep, timer = newTimer}
  where
    newDutyStep = if timer p == 0 then (dutyStep p - 1) `mod` 8 else dutyStep p
    newTimer = if timer p == 0 then period p else timer p - 1

tickSweepUnit :: Pulse -> Pulse
tickSweepUnit p = p2
  where
    sweep = sweepUnit p
    p1 =
        if dividerCounter sweep == 0 && enabled sweep && shiftCount sweep > 0
            then
                -- If sweep unit is not muting channel
                if period p >= 8 && targetPeriod sweep <= 0x7ff
                    then
                        updateTargetPeriod $ p{period = targetPeriod sweep}
                    else
                        modifySweep
                            (\s -> s{dividerCounter = dividerPeriod s})
                            p
            else p
    p2 =
        -- TODO Not sure if should use p1 or p2
        if (reloadFlag . sweepUnit) p1 || (dividerCounter . sweepUnit) p1 == 0
            then
                modifySweep
                    (\s -> s{dividerCounter = dividerPeriod s, reloadFlag = False})
                    p1
            else modifySweep (\s -> s{dividerCounter = dividerCounter s - 1}) p1

--

instance HasLengthCounter Pulse where
    getLengthCounter = lengthCounter
    setLengthCounter lc a = a{lengthCounter = lc}

instance HasEnvelope Pulse where
    getEnvelope = envelope
    setEnvelope e a = a{envelope = e}

{-# INLINE getPulseOutput #-}
getPulseOutput :: Pulse -> Int
getPulseOutput p =
    let dutyValue = fromMaybe 0 ((dutySequences !? dutyIndex p) >>= (!? dutyStep p))
        periodOverflows = (targetPeriod . sweepUnit) p > 0x7ff
        isSilenced =
            isSilencedByLengthCounter p
                || (period p < 8)
                || dutyValue == 0
                || periodOverflows
     in if isSilenced
            then 0
            else getEnvelopeOutput (envelope p)

dutySequences :: [[Int]]
dutySequences =
    [ [0, 1, 0, 0, 0, 0, 0, 0]
    , [0, 1, 1, 0, 0, 0, 0, 0]
    , [0, 1, 1, 1, 0, 0, 0, 0]
    , [1, 0, 0, 1, 1, 1, 1, 1]
    ]
