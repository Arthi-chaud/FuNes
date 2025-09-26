module Internal (withProgram, withState, withMemorySetup, withStateAndMemorySetup, withoutTick) where

import Control.Monad
import Data.Word
import Nes.Bus
import Nes.CPU.Interpreter (runProgram)
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
    (Bus -> IO ()) ->
    -- | Continuation, run at the end of the program
    (CPUState -> Bus -> IO ()) ->
    IO ()
withStateAndMemorySetup program st memSetup post = do
    bus <- newBus unsafeEmptyRom
    loadProgramToMemory program bus
    memSetup bus
    -- Not we do not read 0xfffc because it's out of the bus read
    (st'', ticks) <- runProgram st bus (pure ())
    post st'' (bus{cycles = ticks})

-- | Runs a program and returns the state of the CPU at the end of the execution
withProgram :: [Word8] -> (CPUState -> IO ()) -> IO ()
withProgram program cont = withStateAndMemorySetup program newCPUState (\_ -> return ()) (\st _ -> cont st)

withState :: [Word8] -> CPUState -> (CPUState -> IO ()) -> IO ()
withState program st cont = withStateAndMemorySetup program st (\_ -> return ()) (\st' _ -> cont st')

withMemorySetup :: [Word8] -> (Bus -> IO ()) -> (CPUState -> Bus -> IO ()) -> IO ()
withMemorySetup program = withStateAndMemorySetup program newCPUState

-- | Writes the given program into memory using the Bus
loadProgramToMemory :: [Word8] -> Bus -> IO ()
loadProgramToMemory program bus = do
    forM_ (zip program [0 ..]) $
        \(byte, idx) -> writeByte (Byte byte) (Addr idx) bus

withoutTick :: CPU r a -> CPU r a
withoutTick (MkCPU f) = MkCPU $ \st bus cont -> do
    f st bus $ \st' _ -> cont st' bus
