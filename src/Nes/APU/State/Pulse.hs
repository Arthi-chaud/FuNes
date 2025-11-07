module Nes.APU.State.Pulse (
    -- * Pulse
    Pulse (..),
    newPulse,
    clockPulse,
    modifySweep,
    withSweep,

    -- * Sweep Unit
    SweepUnit (..),
    clockSweepUnit,
    updateTargetPeriod,

    -- * Output
    getOutput,
) where

import Data.Bits
import Data.List ((!?))
import Data.Maybe (fromMaybe)
import Nes.APU.State.LengthCounter

data Pulse = MkP
    { dutyIndex :: Int
    -- ^ Index for the 'dutySequences' table
    , dutyStep :: Int
    -- ^ Index for a row's element in the 'dutySequences' table
    , lengthCounter :: LengthCounter
    , period :: Int
    -- ^ Max value of the timer
    , timer :: Int
    -- ^ Decreases each tick, from 'period' to 0 and loops
    , volume :: Int
    , volumeIsConstant :: Bool
    , sweepUnit :: SweepUnit
    }

-- | Args is true if building pulse 1
newPulse :: Bool -> Pulse
newPulse isPulseOne = MkP 0 0 newLengthCounter 0 0 0 False (MkSU False 0 0 False 0 0 False isPulseOne)

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

modifySweep :: (SweepUnit -> SweepUnit) -> Pulse -> Pulse
modifySweep f p = p{sweepUnit = f (sweepUnit p)}

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

clockPulse :: Pulse -> Pulse
clockPulse p = p{dutyStep = newDutyStep, timer = newTimer}
  where
    newDutyStep = if timer p == 0 then (dutyStep p + 1) `mod` 8 else dutyStep p
    newTimer = if timer p == 0 then period p else timer p - 1

clockSweepUnit :: Pulse -> Pulse
clockSweepUnit p = p2
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

getOutput :: Pulse -> Int
getOutput p =
    let dutyValue = fromMaybe 0 ((dutySequences !? dutyIndex p) >>= (!? dutyStep p))
        periodOverflows = (targetPeriod . sweepUnit) p > 0x7ff
        isSilenced =
            isSilencedByLengthCounter p
                || (period p < 8)
                || dutyValue == 0
                || periodOverflows
     in if isSilenced
            then 0
            else volume p

dutySequences :: [[Int]]
dutySequences =
    [ [0, 1, 0, 0, 0, 0, 0, 0]
    , [0, 1, 1, 0, 0, 0, 0, 0]
    , [0, 1, 1, 1, 0, 0, 0, 0]
    , [1, 0, 0, 1, 1, 1, 1, 1]
    ]
