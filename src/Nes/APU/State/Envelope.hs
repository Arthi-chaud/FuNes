module Nes.APU.State.Envelope (
    -- * Type
    Envelope (..),
    newEnvelope,

    -- * Type class
    HasEnvelope (..),
    withEnvelope,

    -- * Clock
    clockEnvelope,

    -- * Output
    getEnvelopeOutput,
) where

data Envelope = MkE
    { startFlag :: Bool
    , useConstantVolume :: Bool
    , constantVolume :: Int
    , decayLevel :: Int
    , divider :: Int
    , loopFlag :: Bool
    }

newEnvelope :: Envelope
newEnvelope = MkE False False 0 0 0 False

class HasEnvelope a where
    getEnvelope :: a -> Envelope
    setEnvelope :: Envelope -> a -> a

withEnvelope :: (HasEnvelope a) => (Envelope -> Envelope) -> a -> a
withEnvelope f a = setEnvelope (f $ getEnvelope a) a

clockEnvelope :: Envelope -> Envelope
clockEnvelope e =
    if startFlag e
        then e{startFlag = False, decayLevel = 15, divider = constantVolume e}
        else clockDivider e

clockDivider :: Envelope -> Envelope
clockDivider e =
    if divider e == 0
        then e{divider = constantVolume e, decayLevel = newDecay}
        else e{divider = divider e - 1}
  where
    newDecay =
        if decayLevel e == 0
            then if loopFlag e then 15 else 0
            else decayLevel e - 1

getEnvelopeOutput :: Envelope -> Int
getEnvelopeOutput e =
    if useConstantVolume e
        then constantVolume e
        else decayLevel e
