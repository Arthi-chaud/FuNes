module CPU.TraceSpec (spec) where

import Control.Monad (forM)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map as M
import qualified Data.Map as Map
import Nes.Bus (newBus)
import Nes.CPU.Instructions.Map
import Nes.CPU.Interpreter (runProgram)
import Nes.CPU.Monad
import Nes.CPU.State (CPUState (..), newCPUState)
import Nes.Memory (Addr (Addr, unAddr), Byte (unByte), MemoryInterface (readAddr, readByte))
import Nes.Rom (fromFile)
import Test.Hspec
import Text.Printf (printf)

-- TODO Full test
-- TODO 2 tests from guide

spec :: Spec
spec = it "Trace should match" $ do
    expectedTrace <- loadExpectedRawTrace
    rom <- do
        eitherRom <- fromFile "test/assets/rom.nes"
        either fail return eitherRom
    bus <- newBus rom
    traceRef <- newIORef (T [] 0)
    _ <- runProgram newCPUState bus (trace traceRef)
    actualTrace <- toRawTrace <$> readIORef traceRef
    actualTrace `shouldBe` expectedTrace

data Trace = T {entriesStack :: ![String], size :: !Int}

type RawTrace = [ByteString]

toRawTrace :: Trace -> RawTrace
toRawTrace (T stack _) = pack <$> reverse stack

trace :: IORef Trace -> CPU r ()
trace traceRef = do
    newEntry <- getTrace
    liftIO $ modifyIORef traceRef (push newEntry)
  where
    push :: String -> Trace -> Trace
    push s (T list len) = T (s : list) (len + 1)

getTrace :: CPU r String
getTrace = do
    pc <- getPC
    opcodeByte <- withBus $ readByte pc
    opcodeInfo <- getOpcodeInfo opcodeByte
    undefined
  where
    getOpcodeInfo byte = case M.lookup byte opcodeMap of
        Nothing -> fail "Unknown opcode"
        Just n -> return n

getPCTrace :: CPU r String
getPCTrace = printf "%04X" . unAddr <$> getPC

getOpCodeTrace :: CPU r String
getOpCodeTrace = do
    pc <- getPC
    opcodeByte <- withBus $ readByte pc
    opcodeInfo <-
        maybe
            (fail $ printf "Unknown opcode: %02x" $ unByte opcodeByte)
            return
            (Map.lookup opcodeByte opcodeMap)
    instrArgs <- forM [1 .. 0] $ \offset -> withBus $ readByte (Addr $ unAddr pc + offset)
    undefined

getCPUStateTrace :: CPU r String
getCPUStateTrace = withCPUState $ \st ->
    printf
        "A:%02x X:%02x Y:%02x P:%02x SP:%02x"
        (unByte $ registerA st)
        (unByte $ registerX st)
        (unByte $ registerY st)
        (unByte $ status st)
        (unByte $ registerS st)

loadExpectedRawTrace :: IO RawTrace
loadExpectedRawTrace = do
    fileContent <- BS.readFile "test/assets/rom_trace.log"
    let rawTrace = BS.lines fileContent
    -- TODO When project is finished, we shouldn't have to do the following filters
    return $ withoutCycles $ beforeUnofficialInstr rawTrace
  where
    beforeUnofficialInstr =
        takeWhile
            (\line -> not $ "C6BD" `BS.isPrefixOf` line)
    withoutCycles = fmap (fst . BS.breakSubstring " PPU:")
