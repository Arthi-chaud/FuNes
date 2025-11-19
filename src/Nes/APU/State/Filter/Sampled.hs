{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Nes.APU.State.Filter.Sampled (SampledFilter (..), newSampledFilter) where

import Control.Monad.IO.Class
import Nes.APU.State.Filter.Class
import Nes.APU.State.Filter.Constants
import Nes.APU.State.Filter.Fir
import Nes.APU.State.Filter.Iir
import Prelude hiding (filter)

data SampledFilter = MkSF
    { filter :: Either IirFilter FirFilter
    , samplePeriod :: {-# UNPACK #-} !Float
    , periodCounter :: {-# UNPACK #-} !Float
    }

newSampledFilter :: Either IirFilter FirFilter -> SampleRate -> SampledFilter
newSampledFilter filter sampleRate = MkSF{..}
  where
    periodCounter = 1
    samplePeriod = 1 / sampleRate

instance (MonadIO m) => Filter m SampledFilter where
    consume = sampledFilterConsumeSample
    output sf = output $ filter sf

{-# INLINE sampledFilterConsumeSample #-}
sampledFilterConsumeSample :: (MonadIO m) => Sample -> SampledFilter -> m SampledFilter
sampledFilterConsumeSample sample sf = do
    filter' <- consume sample $ filter sf
    return
        sf
            { filter = filter'
            }
