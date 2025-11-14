{-# LANGUAGE RecordWildCards #-}

module Nes.APU.State.Filter.Chain (FilterChain (..), newFilterChain) where

import Nes.APU.State.Filter.Class
import Nes.APU.State.Filter.Constants
import Nes.APU.State.Filter.Fir
import Nes.APU.State.Filter.Iir
import Nes.APU.State.Filter.Sampled
import Prelude hiding (filter)

data FilterChain = MkFC
    { filters :: ![SampledFilter]
    , dt :: {-# UNPACK #-} !Float
    }

newFilterChain :: OutputRate -> FilterChain
newFilterChain outputRate = MkFC{..}
  where
    clockRate = 21477272 / 12
    intermediateSampleRate = outputRate * 2 + (pi / 32)
    intermediateCutoff = outputRate * 0.4
    dt = 1 / clockRate
    filters =
        [ newSampledFilter (Left identityIirFilter) 1.0
        , newSampledFilter (Left $ lowPassIirFilter clockRate intermediateCutoff) clockRate
        , newSampledFilter (Left $ highPassIirFilter intermediateSampleRate 90) intermediateSampleRate
        , newSampledFilter (Left $ highPassIirFilter intermediateSampleRate 440) intermediateSampleRate
        , newSampledFilter (Left $ lowPassIirFilter intermediateSampleRate 14000) intermediateSampleRate
        , newSampledFilter (Right $ lowPassFirFilter intermediateSampleRate (outputRate * 0.45) 160) intermediateSampleRate
        ]

instance Filter FilterChain where
    consume = filterChainConsumeSample
    output = filterChainOutput

filterChainConsumeSample :: Sample -> FilterChain -> FilterChain
filterChainConsumeSample sample fc =
    let
        fc1 = modifyFilterAtIndex 0 (consume sample) fc
        updatedFilters = go (filters fc1) (dt fc1)
     in
        fc1{filters = updatedFilters}
  where
    go :: [SampledFilter] -> Float -> [SampledFilter]
    go [] _ = []
    go [a] _ = [a]
    go (prev : curr : rest) dt =
        let
            newCurr = filterChainConsumeIteration prev curr dt
         in
            prev : go (newCurr : rest) dt

filterChainConsumeIteration :: SampledFilter -> SampledFilter -> Float -> SampledFilter
filterChainConsumeIteration prev current dt =
    if periodCounter current >= samplePeriod current
        then
            let
                newPeriodCounter = periodCounter current - samplePeriod current
                previousOutput = output $ filter prev
                newCurrent = consume previousOutput $ current{periodCounter = newPeriodCounter}
             in
                filterChainConsumeIteration
                    prev
                    newCurrent
                    dt
        else
            let newPeriodCounter = periodCounter current + dt
             in current{periodCounter = newPeriodCounter}

{-# INLINE modifyFilterAtIndex #-}
modifyFilterAtIndex :: Int -> (SampledFilter -> SampledFilter) -> FilterChain -> FilterChain
modifyFilterAtIndex idx f fc = case splitAt idx $ filters fc of
    (_, []) -> fc
    (left, item : right) -> fc{filters = left ++ (f item : right)}

{-# INLINE filterChainOutput #-}
filterChainOutput :: FilterChain -> Sample
filterChainOutput fc = case filters fc of
    [] -> 0
    l -> either output output . filter $ last l
