module Nes.APU.State.FrameCounter (
    FrameCounter (..),
    newFrameCounter,

    -- * Sequence mode
    SequenceMode (..),
    sequenceModeFromBool,

    -- * Utils
    shouldIncrementSequenceStep,
    shouldResetSequenceStep,
    incrementSequenceStep,
    resetSequence,
    setCycles,
) where

import Data.List ((!?))

data SequenceMode = FourStep | FiveStep deriving (Eq, Show, Enum)

{-# INLINE sequenceModeFromBool #-}
sequenceModeFromBool :: Bool -> SequenceMode
sequenceModeFromBool = toEnum . fromEnum

{-# INLINE sequenceModeStepCount #-}
sequenceModeStepCount :: SequenceMode -> Int
sequenceModeStepCount = \case
    FourStep -> 4
    FiveStep -> 5

sequenceStepCycles :: SequenceMode -> [Int]
sequenceStepCycles = \case
    FourStep -> [3728, 7456, 11185, 14914, 14915]
    FiveStep -> [3728, 7456, 11185, 14914, 18640, 18641]

{-# INLINE shouldIncrementSequenceStep #-}
shouldIncrementSequenceStep :: FrameCounter -> Bool
shouldIncrementSequenceStep fc =
    let
        table = sequenceStepCycles $ sequenceMode fc
     in
        case table !? sequenceStep fc of
            Nothing -> False
            Just s -> cycles fc >= s

{-# INLINE shouldResetSequenceStep #-}
shouldResetSequenceStep :: FrameCounter -> Bool
shouldResetSequenceStep fc =
    let
        table = sequenceStepCycles $ sequenceMode fc
     in
        case table !? 5 of
            Nothing -> False
            Just s -> cycles fc >= s

data FrameCounter = MkFC
    { sequenceMode :: {-# UNPACK #-} !SequenceMode
    , frameInterruptFlag :: {-# UNPACK #-} !Bool
    , inhibitInterrupt :: {-# UNPACK #-} !Bool
    , sequenceStep :: {-# UNPACK #-} !Int
    , cycles :: {-# UNPACK #-} !Int
    , delayedWriteSideEffectCycle :: !(Maybe Int)
    }

newFrameCounter :: FrameCounter
newFrameCounter = MkFC FourStep False False 0 0 Nothing

{-# INLINE resetSequence #-}
resetSequence :: FrameCounter -> FrameCounter
resetSequence fc = fc{cycles = 0, sequenceStep = 0}

{-# INLINE setCycles #-}
setCycles :: (Int -> Int) -> FrameCounter -> FrameCounter
setCycles f fc = fc{cycles = f $ cycles fc}

-- | Increment 'sequenceStep', or set to zero when sequence ends
{-# INLINE incrementSequenceStep #-}
incrementSequenceStep :: FrameCounter -> FrameCounter
incrementSequenceStep fc =
    fc
        { sequenceStep = nextStep `mod` (maxStep + 1)
        -- To get end of sequence
        }
  where
    nextStep = sequenceStep fc + 1
    maxStep = sequenceModeStepCount (sequenceMode fc)
