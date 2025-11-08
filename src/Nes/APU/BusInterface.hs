module Nes.APU.BusInterface (
    writeToAPU,
) where

import Nes.APU.BusInterface.FrameCounter
import Nes.APU.BusInterface.Noise
import Nes.APU.BusInterface.Pulse
import Nes.APU.BusInterface.Status
import Nes.APU.BusInterface.Triangle
import Nes.APU.Monad
import Nes.Memory (Addr, Byte (..))

writeToAPU :: Addr -> Byte -> APU r ()
writeToAPU addr = case addr of
    -- Pulse 1
    0x4000 -> write4000
    0x4001 -> write4001
    0x4002 -> write4002
    0x4003 -> write4003
    -- Pulse 2
    0x4004 -> write4004
    0x4005 -> write4005
    0x4006 -> write4006
    0x4007 -> write4007
    -- Triangle
    0x4008 -> write4008
    0x400A -> write400A
    0x400B -> write400B
    -- Noise
    0x400C -> write400C
    0x400E -> write400E
    0x400F -> write400F
    -- Status
    0x4015 -> write4015
    -- Frame Counter
    0x4017 -> write4017
    -- TODO: 0x4010, 0x4013
    _ -> const (return ())

-- TODO Read from APU
