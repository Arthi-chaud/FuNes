module Nes.CPU.Interpreter (runProgram, runProgramWithState, interpret) where

import Nes.Bus
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.IN
import Nes.CPU.Instructions.LD
import Nes.CPU.Instructions.TA
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory
import Text.Printf

-- TODO rename these 2 functions

-- | Runs the program that's on the memory (interfaced by the given 'Bus').
--
-- Returns the state of the CPU
runProgram :: Bus -> IO CPUState
runProgram = runProgramWithState newCPUState

-- | Same as 'runProgram', but using a given initial state
runProgramWithState :: CPUState -> Bus -> IO CPUState
runProgramWithState state prog = unCPU interpret state prog $ \state' _ _ -> return state'

-- | Interpretation loop of the program
interpret :: CPU r ()
interpret = do
    opCode <- readAtPC
    incrementPC
    if opCode == 0x00
        then return ()
        else go opCode >> interpret
  where
    go = \case
        -- LDA
        0xa9 -> lda Immediate
        0xa5 -> lda ZeroPage
        0xb5 -> lda ZeroPageX
        0xad -> lda Absolute
        0xbd -> lda AbsoluteX
        0xb9 -> lda AbsoluteY
        0xa1 -> lda IndirectX
        0xb1 -> lda IndirectY
        -- TODO
        0xaa -> tax
        0xe8 -> inx
        0x00 -> pure () -- Redundant with the check
        code -> fail $ printf "OP Code not implemented: 0x%x" (unByte code)
