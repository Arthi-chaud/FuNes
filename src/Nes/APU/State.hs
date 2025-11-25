{-# LANGUAGE TemplateHaskell #-}

module Nes.APU.State (
    -- * Definition
    APUState (..),
    newAPUState,

    -- * Lens
    frameCounter,
    pulse1,
    pulse2,
    triangle,
    noise,
    dmc,
    cycle,
    filterThread,
    samplePeriod,
    sampleTimer,
    pushSampleCallback,
) where

import Control.Lens (makeLenses)
import Nes.APU.State.DMC
import Nes.APU.State.Filter.Constants (defaultOutputRate)
import Nes.APU.State.Filter.Thread (FilterThread)
import Nes.APU.State.FrameCounter
import Nes.APU.State.Noise
import Nes.APU.State.Pulse
import Nes.APU.State.Triangle
import Prelude hiding (cycle)

data APUState = MkAPUState
    { _frameCounter :: !FrameCounter
    , _pulse1 :: !Pulse
    , _pulse2 :: !Pulse
    , _triangle :: !Triangle
    , _noise :: !Noise
    , _dmc :: !DMC
    , _cycle :: {-# UNPACK #-} !Int
    -- ^ Number of CPU cycles since the start
    , _filterThread :: !FilterThread
    , _sampleTimer :: {-# UNPACK #-} !Float
    -- ^ The number of CPU cycles since the last call to 'pushSampleCallback'
    , _samplePeriod :: {-# UNPACK #-} !Float
    -- ^ The number of CPU cycles between each call to 'pushSampleCallback'
    , _pushSampleCallback :: Float -> IO ()
    }

newAPUState :: (Float -> IO ()) -> FilterThread -> APUState
newAPUState _pushSampleCallback _filterThread = MkAPUState{..}
  where
    _frameCounter = newFrameCounter
    _cycle = 0
    _pulse1 = newPulse True
    _pulse2 = newPulse False
    _triangle = newTriangle
    _noise = newNoise
    _dmc = newDMC
    _samplePeriod = (21477272 / 12) / defaultOutputRate
    _sampleTimer = _samplePeriod

makeLenses ''APUState
