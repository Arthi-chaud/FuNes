{-# LANGUAGE RecordWildCards #-}

module Nes.APU.State.Filter (
    -- * Filter chain
    FilterChain (..),
    newFilterChain,
    processSample,

    -- * Filter
    lowPassFilter,
    highPassFilter,
    filterProcessSample,
) where

import Prelude hiding (filter)

-- Source: https://github.com/luckasRanarison/mes/blob/main/crates/mes-core/src/apu/filters.rs#L58

newtype FilterChain = MkFC {unFC :: [Filter]}

newFilterChain :: FilterChain
newFilterChain = MkFC [highPassFilter 44100 90, highPassFilter 44100 440, lowPassFilter 44100 14000]

processSample :: Float -> FilterChain -> (Float, FilterChain)
processSample sample chain =
    let
        (res, newChain) =
            foldl
                ( \(sample', newFilters) filter ->
                    let
                        (filteredSample, filter') = filterProcessSample sample' filter
                     in
                        (filteredSample, newFilters ++ [filter'])
                )
                (sample, [])
                $ unFC chain
     in
        (res, MkFC newChain)

data Filter = MkF
    { b0 :: {-# UNPACK #-} !Float
    , b1 :: {-# UNPACK #-} !Float
    , a1 :: {-# UNPACK #-} !Float
    , prevX :: {-# UNPACK #-} !Float
    , prevY :: {-# UNPACK #-} !Float
    }

lowPassFilter :: Float -> Float -> Filter
lowPassFilter sampleRate freq = MkF{..}
  where
    b0 = a0
    b1 = a0
    a1 = (1.0 - c) * a0
    prevX = 0.0
    prevY = 0.0
    c = sampleRate / (freq * pi)
    a0 = 1.0 / (1.0 + c)

highPassFilter :: Float -> Float -> Filter
highPassFilter sampleRate freq = MkF{..}
  where
    b0 = c * a0
    b1 = (-c) * a0
    a1 = (1.0 - c) * a0
    prevX = 0.0
    prevY = 0.0
    c = sampleRate / (freq * pi)
    a0 = 1.0 / (1.0 + c)

{-# INLINE filterProcessSample #-}
filterProcessSample :: Float -> Filter -> (Float, Filter)
filterProcessSample sample f@MkF{..} = (res, newFilter)
  where
    res = b0 * sample + b1 * prevX - a1 * prevY
    newFilter = f{prevX = sample, prevY = res}
