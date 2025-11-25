module Internal (withProgram, withState, withMemorySetup, withStateAndMemorySetup) where

import Control.Monad
import Data.Word
import Nes.APU.State.Filter.Thread
import Nes.Bus.State
import Nes.CPU.Interpreter (runProgram')
import Nes.CPU.Monad (CPU (MkCPU))
import Nes.CPU.State
import Nes.Memory
import Nes.Memory.Unsafe ()
import Nes.Rom

withStateAndMemorySetup ::
    -- | Program
    [Word8] ->
    -- | Initial CPU State
    CPUState ->
    -- | Function that write to memory
    (BusState -> IO r) ->
    -- | Continuation, run at the end of the program
    (CPUState -> BusState -> IO r') ->
    IO ()
withStateAndMemorySetup program st memSetup post = do
    bus <- newBusState unsafeEmptyRom (\_ -> pure ()) pure (\_ -> pure ()) (curry return) newNoopFilterThread
    loadProgramToMemory program bus
    _ <- memSetup bus
    -- Not we do not read 0xfffc because it's out of the bus read
    (st'', ticks) <- runProgram' st bus stopAtBrk
    _ <- post st'' (bus{cycles = ticks})
    return ()
  where
    stopAtBrk = MkCPU $ \st' bus cont -> do
        b <- readByte (_pc st') (cpuVram bus)
        if b == 0x00
            then pure (st', cycles bus)
            else cont st' bus ()

-- | Runs a program and returns the state of the CPU at the end of the execution
withProgram :: [Word8] -> (CPUState -> IO r) -> IO ()
withProgram program cont = withStateAndMemorySetup program newCPUState (\_ -> return ()) (\st _ -> cont st)

withState :: [Word8] -> CPUState -> (CPUState -> IO r) -> IO ()
withState program st cont = withStateAndMemorySetup program st (\_ -> return ()) (\st' _ -> cont st')

withMemorySetup :: [Word8] -> (BusState -> IO r) -> (CPUState -> BusState -> IO r') -> IO ()
withMemorySetup program = withStateAndMemorySetup program newCPUState

-- | Writes the given program into memory using the Bus
loadProgramToMemory :: [Word8] -> BusState -> IO ()
loadProgramToMemory program bus = do
    forM_ (zip program [0 ..]) $
        \(byte, idx) -> writeByte (Byte byte) (Addr idx) (cpuVram bus)
