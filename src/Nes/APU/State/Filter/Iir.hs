{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Nes.APU.State.Filter.Iir (
    IirFilter (..),

    -- * Build predefined filters
    identityIirFilter,
    highPassIirFilter,
    lowPassIirFilter,
) where

import Nes.APU.State.Filter.Class
import Nes.APU.State.Filter.Constants

-- | Infinite impulse response (IIR) filter
data IirFilter = MkIirF
    { alpha :: {-# UNPACK #-} !Float
    , previousOutput :: {-# UNPACK #-} !Sample
    , previousInput :: {-# UNPACK #-} !Sample
    , delta :: {-# UNPACK #-} !Float
    , outputF :: !(IirFilter -> Sample)
    }

identityIirFilter :: IirFilter
identityIirFilter =
    MkIirF
        { alpha = 0
        , previousInput = 0
        , previousOutput = 0
        , delta = 0
        , outputF = previousInput
        }

highPassIirFilter :: SampleRate -> Cutoff -> IirFilter
highPassIirFilter sampleRate cutoff =
    MkIirF
        { alpha = cutoffPeriod / (cutoffPeriod + period)
        , previousOutput = 0
        , previousInput = 0
        , delta = 0
        , outputF = \f -> alpha f * previousOutput f + alpha f * delta f
        }
  where
    period = 1 / sampleRate
    cutoffPeriod = 1 / cutoff

lowPassIirFilter :: SampleRate -> Cutoff -> IirFilter
lowPassIirFilter sampleRate cutoff =
    MkIirF
        { alpha = cutoffPeriod / (cutoffPeriod + period)
        , previousOutput = 0
        , previousInput = 0
        , delta = 0
        , outputF = \f -> previousOutput f + alpha f * delta f
        }
  where
    period = 1 / sampleRate
    cutoffPeriod = 1 / (2 * pi * cutoff)

instance (Monad m) => Filter m IirFilter where
    {-# INLINE output #-}
    output f = return $ outputF f f
    {-# INLINE consume #-}
    consume sample f = do
        prevOut <- output @m f
        return $
            f
                { previousOutput = prevOut
                , delta = sample - previousInput f
                , previousInput = sample
                }
