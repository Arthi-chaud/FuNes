module Nes.CPU.Interpreter (
    runProgram,
    interpret,
    interpretWithCallback,
) where

import Control.Monad
import Data.Map
import Nes.Bus
import Nes.CPU.Instructions.Map
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory
import Text.Printf

-- | Runs the program that's on the memory (interfaced by the given 'Bus') usingthe given CPU state.
--
-- The thrid argument is a callback run at each loop
--
-- Returns the state of the CPU with the number of ellapsed ticks
runProgram :: CPUState -> Bus -> CPU (CPUState, Integer) () -> IO (CPUState, Integer)
runProgram state prog callback = unCPU (interpretWithCallback callback) state prog $ \state' bus _ -> return (state', cycles bus)

interpret :: CPU r ()
interpret = interpretWithCallback $ pure ()

-- | Interpretation loop of the program
interpretWithCallback :: CPU r () -> CPU r ()
interpretWithCallback callback = do
    callback
    oldCycleCount <- getCycles
    opCode <- readAtPC
    incrementPC
    if opCode == 0x00
        then return ()
        else do
            go opCode
            newCycleCount <- getCycles
            -- Each opcode should take at least 2 ticks
            -- We cannot just check that addressing is none,
            -- because some opcode w/o addressing take more that 1 cycle
            -- e.g. php
            when (newCycleCount - 1 == oldCycleCount) tickOnce
            interpretWithCallback callback
  where
    go opcode = case Data.Map.lookup opcode opcodeMap of
        Just (_, f, mode) -> f mode
        Nothing -> fail $ printf "OP Code not implemented: 0x%x" (unByte opcode)
