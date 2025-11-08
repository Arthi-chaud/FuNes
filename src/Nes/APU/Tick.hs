module Nes.APU.Tick (
    -- * Semantic of a tick
    -- $semantic
    tick,
    tickOnce,
    IsAPUCycle,
) where

import Control.Monad
import Nes.APU.Monad
import Nes.APU.Monad.FrameCounter (clockFrameCounter)
import Nes.APU.State (modifyPulse1, modifyPulse2, modifyTriangle)
import Nes.APU.State.Pulse (clockPulse)
import Nes.APU.State.Triangle (clockTriangle)

-- $use
--     The APU being a part of the CPU, they both tick at the same time. However, some clocks are updated every other CPU cycles.
--     Here the 'tick' function should be called every CPU tick, and pass as parameter whether the tick is on an even CPU cycle or not.
--     Same goes for 'tickMany'.

type IsAPUCycle = Bool

-- | Calls 'tick' n amount of time
--
-- the first parameter says whether the first tick is an APU cycle or not
tick :: IsAPUCycle -> Int -> APU r ()
tick _ 0 = return ()
tick b n = tickOnce b >> tick (not b) (n - 1)

tickOnce :: IsAPUCycle -> APU r ()
tickOnce isAPUCycle = do
    -- TODO Ticks and clocks
    modifyAPUState $ modifyTriangle clockTriangle
    when isAPUCycle $ do
        modifyAPUState $
            modifyPulse1 clockPulse
                . modifyPulse2 clockPulse
        clockFrameCounter
