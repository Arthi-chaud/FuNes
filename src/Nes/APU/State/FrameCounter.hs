module Nes.APU.State.FrameCounter (
    FrameCounter (..),
    newFrameCounter,

    -- * Sequence mode
    SequenceMode (..),
    sequenceModeFromBool,

    -- * Sequence step
    incrementSequenceStep,
) where

data SequenceMode = FourStep | FiveStep deriving (Eq, Show, Enum)

sequenceModeFromBool :: Bool -> SequenceMode
sequenceModeFromBool = toEnum . fromEnum

sequenceModeStepCount :: SequenceMode -> Int
sequenceModeStepCount = \case
    FourStep -> 4
    FiveStep -> 5

data FrameCounter = MkFC
    { sequenceMode :: SequenceMode
    , frameInterruptFlag :: Bool
    , inhibitInterrupt :: Bool
    , sequenceStep :: Int
    }

newFrameCounter :: FrameCounter
newFrameCounter = MkFC FourStep False False 0

-- | Increment 'sequenceStep', or set to zero when sequence ends
incrementSequenceStep :: FrameCounter -> FrameCounter
incrementSequenceStep fc =
    fc
        { sequenceStep = nextStep `mod` maxStep
        }
  where
    nextStep = sequenceStep fc + 1
    maxStep = sequenceModeStepCount (sequenceMode fc)
