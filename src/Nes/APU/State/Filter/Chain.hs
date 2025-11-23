module Nes.APU.State.Filter.Chain (FilterChain (..), newFilterChain) where

import Control.Monad
import qualified Data.Vector.Mutable as V
import Nes.APU.State.Filter.Class
import Nes.APU.State.Filter.Constants
import Nes.APU.State.Filter.Fir
import Nes.APU.State.Filter.Iir
import Nes.APU.State.Filter.Sampled
import Prelude hiding (filter)

data FilterChain = MkFC
    { filters :: !(V.IOVector SampledFilter)
    , dt :: {-# UNPACK #-} !Float
    }

newFilterChain :: OutputRate -> IO FilterChain
newFilterChain outputRate = do
    filtersList <- do
        _firFilter <- lowPassFirFilter intermediateSampleRate (outputRate * 0.45) 160
        return
            [ newSampledFilter (Left identityIirFilter) 1.0
            , newSampledFilter (Left $ lowPassIirFilter clockRate intermediateCutoff) clockRate
            , newSampledFilter (Left $ highPassIirFilter intermediateSampleRate 90) intermediateSampleRate
            , newSampledFilter (Left $ highPassIirFilter intermediateSampleRate 440) intermediateSampleRate
            , newSampledFilter (Left $ lowPassIirFilter intermediateSampleRate 14000) intermediateSampleRate
            -- , newSampledFilter (Right firFilter) intermediateSampleRate
            ]
    filters <- V.new $ length filtersList
    forM_ (zip [0 ..] filtersList) $ uncurry (V.write filters)
    return MkFC{..}
  where
    clockRate = 21477272 / 12
    intermediateSampleRate = outputRate * 2 + (pi / 32)
    intermediateCutoff = outputRate * 0.4
    dt = 1 / clockRate

instance Filter IO FilterChain where
    consume = filterChainConsumeSample
    output = filterChainOutput

filterChainConsumeSample :: Sample -> FilterChain -> IO FilterChain
filterChainConsumeSample sample fc = do
    V.modifyM (filters fc) (consume sample) 0
    firstFilter <- V.read (filters fc) 0
    _ <-
        V.ifoldM'
            ( \prev currIdx curr ->
                if currIdx == 0
                    then return curr
                    else do
                        !newCurr <- filterChainConsumeIteration prev (dt fc) curr
                        V.write (filters fc) currIdx newCurr
                        return newCurr
            )
            firstFilter
            (filters fc)
    return fc

filterChainConsumeIteration :: SampledFilter -> Float -> SampledFilter -> IO SampledFilter
filterChainConsumeIteration prev dt current =
    if periodCounter current >= samplePeriod current
        then do
            let
                newPeriodCounter = periodCounter current - samplePeriod current
            previousOutput <- output $ filter prev
            newCurrent <- consume previousOutput $ current{periodCounter = newPeriodCounter}
            filterChainConsumeIteration
                prev
                dt
                newCurrent
        else
            let newPeriodCounter = periodCounter current + dt
             in return $ current{periodCounter = newPeriodCounter}

{-# INLINE filterChainOutput #-}
filterChainOutput :: FilterChain -> IO Sample
filterChainOutput fc = case V.length $ filters fc of
    0 -> return 0
    l -> either (output @IO) (output @IO) . filter =<< V.read (filters fc) (l - 1)
