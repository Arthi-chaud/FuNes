module Nes.APU.State.Filter.Constants (
    -- * Constants
    defaultOutputRate,

    -- * Type alias
    Sample,
    SampleRate,
    Cutoff,
    OutputRate,
) where

type Sample = Float
type SampleRate = Float
type Cutoff = Float
type OutputRate = Float

defaultOutputRate :: OutputRate
defaultOutputRate = 44100
