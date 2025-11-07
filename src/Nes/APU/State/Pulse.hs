module Nes.APU.State.Pulse (Pulse (..), newPulse, clockPulse, getOutput) where

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
    }

-- TODO Sweep unit

newPulse :: Pulse
newPulse = MkP 0 0 newLengthCounter 0 0 0 False

clockPulse :: Pulse -> Pulse
clockPulse p = p{dutyStep = newDutyStep, timer = newTimer}
  where
    newDutyStep = if timer p == 0 then (dutyStep p + 1) `mod` 8 else dutyStep p
    newTimer = if timer p == 0 then period p else timer p - 1

instance HasLengthCounter Pulse where
    getLengthCounter = lengthCounter
    setLengthCounter lc a = a{lengthCounter = lc}

getOutput :: Pulse -> Int
getOutput p =
    let dutyValue = fromMaybe 0 ((dutySequences !? dutyIndex p) >>= (!? dutyStep p))
        isSilenced = isSilencedByLengthCounter p || (period p < 8) || dutyValue == 0
     in -- TODO return 0 if overflow from the sweep unit's adder is silencing the channel
        if isSilenced
            then 0
            else volume p

dutySequences :: [[Int]]
dutySequences =
    [ [0, 1, 0, 0, 0, 0, 0, 0]
    , [0, 1, 1, 0, 0, 0, 0, 0]
    , [0, 1, 1, 1, 0, 0, 0, 0]
    , [1, 0, 0, 1, 1, 1, 1, 1]
    ]
