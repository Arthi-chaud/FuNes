{-# LANGUAGE RecordWildCards #-}

module Nes.APU.State.Triangle (
    -- * Definition
    Triangle (..),
    newTriangle,

    -- * Output
    getTriangleOutput,

    -- * Clock
    clockTriangle,
    clockTriangleLinearCounter,
) where

import Nes.APU.State.LengthCounter

data Triangle = MkT
    { controlFlag :: Bool
    , reloadFlag :: Bool
    , reloadValue :: Int
    , lengthCounter :: LengthCounter
    , linearCounter :: Int
    , period :: Int
    , timer :: Int
    , sequenceStep :: Int
    }

newTriangle :: Triangle
newTriangle = MkT{..}
  where
    controlFlag = False
    reloadFlag = False
    reloadValue = 0
    lengthCounter = newLengthCounter
    period = 0
    linearCounter = 0
    sequenceStep = 0
    timer = 0

{-# INLINE getSequenceValue #-}
getSequenceValue :: Triangle -> Int
getSequenceValue t = if step <= 15 then 15 - step else step - 16
  where
    step = sequenceStep t

getTriangleOutput :: Triangle -> Int
getTriangleOutput t = if remainingLength (lengthCounter t) /= 0 then getSequenceValue t else 0

instance HasLengthCounter Triangle where
    getLengthCounter = lengthCounter
    setLengthCounter lc t = t{lengthCounter = lc}

clockTriangle :: Triangle -> Triangle
clockTriangle t = t{timer = newTimer, sequenceStep = newSequenceStep}
  where
    newTimer = if timer t == 0 then period t else timer t - 1
    clockSequence = timer t == 0 && linearCounter t /= 0 && remainingLength (lengthCounter t) /= 0
    newSequenceStep = if clockSequence then (sequenceStep t + 1) `mod` 32 else sequenceStep t

clockTriangleLinearCounter :: Triangle -> Triangle
clockTriangleLinearCounter t = t2
  where
    t1 =
        if reloadFlag t
            then t{linearCounter = reloadValue t}
            else t{linearCounter = max 0 (linearCounter t - 1)}
    t2 =
        if not $ controlFlag t1
            then t1{reloadFlag = False}
            else t1
