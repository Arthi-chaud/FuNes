{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    , pass :: {-# UNPACK #-} !IirFilterPass
    }

data IirFilterPass = Identity | LowPass | HighPass deriving (Eq)

identityIirFilter :: IirFilter
identityIirFilter =
    MkIirF
        { alpha = 0
        , previousInput = 0
        , previousOutput = 0
        , delta = 0
        , pass = Identity
        }

highPassIirFilter :: SampleRate -> Cutoff -> IirFilter
highPassIirFilter sampleRate cutoff =
    MkIirF
        { alpha = cutoffPeriod / (cutoffPeriod + period)
        , previousOutput = 0
        , previousInput = 0
        , delta = 0
        , pass = HighPass
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
        , pass = LowPass
        }
  where
    period = 1 / sampleRate
    cutoffPeriod = 1 / (2 * pi * cutoff)

instance (Monad m) => Filter m IirFilter where
    {-# INLINE output #-}
    output f = return $ case pass f of
        Identity -> previousInput f
        LowPass -> previousOutput f + alpha f * delta f
        HighPass -> alpha f * previousOutput f + alpha f * delta f

    {-# INLINE consume #-}
    consume sample f = do
        prevOut <- output @m f
        return $
            f
                { previousOutput = prevOut
                , delta = sample - previousInput f
                , previousInput = sample
                }
