module Nes.APU.State where

import Nes.APU.State.Pulse (Pulse)

data APUState = MkAPUState {pulse1 :: Pulse, pulse2 :: Pulse}
