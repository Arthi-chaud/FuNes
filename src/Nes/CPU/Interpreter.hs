module Nes.CPU.Interpreter (
    runProgram,
    runProgram',
    interpretWithCallback,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Data.Map
import Nes.Bus
import Nes.Bus.Monad (withPPU)
import Nes.CPU.Instructions.Map
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Interrupt
import Nes.Memory
import Nes.PPU.Monad (modifyPPUState, withPPUState)
import Nes.PPU.State (PPUState (nmiInterrupt))
import Text.Printf

-- | Runs the program that's on the memory (interfaced by the given 'Bus').
--
-- The second argument is a callback run at each loop.
--
-- Returns the state of the CPU with the number of ellapsed ticks
runProgram :: Bus -> CPU (CPUState, Integer) () -> IO (CPUState, Integer)
runProgram prog callback = unCPU
    (reset >> interpretWithCallback callback)
    newCPUState
    prog
    $ \state' bus _ -> return (state', cycles bus)

-- | Same as 'runProgram', but uses a given state
runProgram' :: CPUState -> Bus -> CPU (CPUState, Integer) () -> IO (CPUState, Integer)
runProgram' state prog callback = unCPU (interpretWithCallback callback) state prog $ \state' bus _ -> return (state', cycles bus)

-- | Interpretation loop of the program
interpretWithCallback :: CPU r () -> CPU r ()
interpretWithCallback callback = do
    hasNmiInterrupt <-
        withBus
            ( withPPU $ do
                f <- withPPUState nmiInterrupt
                modifyPPUState $ \st -> st{nmiInterrupt = False}
                return f
            )
    when hasNmiInterrupt $ interrupt NMI
    callback
    oldCycleCount <- getCycles
    opCode <- readAtPC
    incrementPC
    do
        forceMultiByte <- go opCode
        newCycleCount <- getCycles
        -- Each opcode should take at least 2 ticks
        -- We cannot just check that addressing is none,
        -- because some opcode w/o addressing take more that 1 cycle
        -- e.g. php
        -- This does not apply to unofficial KIL/JAM opcodes
        when (forceMultiByte && newCycleCount - 1 == oldCycleCount) tickOnce
        interpretWithCallback callback
  where
    go opcode = case Data.Map.lookup opcode opcodeMap of
        Just (mnemo, f, mode, _) -> f mode $> (mnemo /= "KIL")
        Nothing -> liftIO $ printf "OP Code not implemented: 0x%x\n" (unByte opcode) $> False
