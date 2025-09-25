{-# LANGUAGE TypeApplications #-}

module CPU.TraceSpec (spec) where

import Control.Exception
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map as Map
import Nes.Bus (newBus)
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.Map
import Nes.CPU.Interpreter (runProgram)
import Nes.CPU.Monad
import Nes.CPU.State (CPUState (..), Register (..), newCPUState)
import Nes.Memory (Addr (Addr, unAddr), Byte (unByte), MemoryInterface (readByte), addrToInt, byteToAddr, readAddr)
import Nes.Rom (fromFile)
import Test.Hspec
import Text.Printf (printf)

-- TODO 2 tests from guide

spec :: Spec
spec = it "Trace should match logfile" $ do
    expectedTrace <- loadExpectedRawTrace
    -- BS.writeFile "expected.log" $ BS.unlines expectedTrace
    rom <- do
        eitherRom <- fromFile "test/assets/rom.nes"
        either fail return eitherRom
    bus <- newBus rom
    traceRef <- newIORef (T [] 0)
    let st = newCPUState{programCounter = 0xc000}
    _ <- try @IOException $ runProgram st bus (trace traceRef)
    actualTrace <- toRawTrace <$> readIORef traceRef
    -- BS.writeFile "actual.log" $ BS.unlines actualTrace
    length actualTrace `shouldBe` length expectedTrace
    forM_ [0 .. length expectedTrace - 1] $ \i -> do
        let expected = expectedTrace !! i
        let actual = actualTrace !! i
        actual `shouldBe` expected

data Trace = T ![String] !Int

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
    pc <- getPCTrace
    opcode <- getOpCodeTrace
    st <- getCPUStateTrace
    return $ unwords [pc, opcode, st]

getPCTrace :: CPU r String
getPCTrace = printf "%04X " . unAddr <$> getPC

getOpCodeTrace :: CPU r String
getOpCodeTrace = do
    pc <- getPC
    opcodeByte <- withBus $ readByte pc
    (opname, _, addressing) <-
        maybe
            (fail $ printf "Unknown opcode: %02X" $ unByte opcodeByte)
            return
            (Map.lookup opcodeByte opcodeMap)
    instrArgs <- forM [1 .. (getOperandSize addressing)] $
        \offset -> withBus $ readByte (Addr $ unAddr pc + fromIntegral offset)
    let fmtBytesList = unwords (printf "%02X" . unByte <$> (opcodeByte : instrArgs))
    asm <- do
        incrementPC
        asm <- getOpCodeAsmArg opcodeByte (pc + 1) addressing
        modifyCPUState $ \st -> st{programCounter = pc}
        return asm
    return $ printf "%-8s  %s %-27s" fmtBytesList (BS.unpack opname) asm

getOpCodeAsmArg :: Byte -> Addr -> AddressingMode -> CPU r String
getOpCodeAsmArg opcode ptr addressing = do
    (memAddr, storedVal) <- getMemAddrAndStoredValue
    addressByte <- unByte <$> withBus (readByte ptr)
    addressAddr <- unAddr <$> withBus (readAddr ptr)

    x <- getRegister X
    y <- getRegister Y
    case opcode of
        0x4c -> return $ printf "$%04X" memAddr
        0x20 -> return $ printf "$%04X" memAddr
        0x6c -> do
            jmpAddr <-
                if addressAddr .&. 0x00ff == 0x00ff
                    then do
                        low <- fromIntegral . unByte <$> withBus (readByte $ Addr addressAddr)
                        high <- fromIntegral . unByte <$> withBus (readByte $ Addr addressAddr .&. 0xff00)
                        return $ high `shiftL` 8 .|. low
                    else withBus $ readAddr $ Addr addressAddr
            return $ printf "($%04X) = %04X" addressAddr $ unAddr jmpAddr
        _ -> return $ case addressing of
            Accumulator -> "A "
            Immediate -> printf "#$%02X" addressByte
            ZeroPage -> printf "$%02X = %02X" memAddr storedVal
            ZeroPageX -> printf "$%02X,X @ %02X = %02X" addressByte memAddr storedVal
            ZeroPageY -> printf "$%02X,Y @ %02X = %02X" addressByte memAddr storedVal
            IndirectX ->
                printf
                    "($%02X,X) @ %02X = %04X = %02X"
                    addressByte
                    (addressByte + unByte x)
                    memAddr
                    storedVal
            IndirectY ->
                printf
                    "($%02X),Y = %04X @ %04X = %02X"
                    addressByte
                    (unAddr $ Addr memAddr - byteToAddr y)
                    memAddr
                    storedVal
            Relative -> printf "$%04X" $ (addrToInt ptr + 1) + fromIntegral addressByte
            Absolute -> printf "$%04X = %02X" memAddr storedVal
            AbsoluteX -> printf "$%04X,X @ %04X = %02X" addressAddr memAddr storedVal
            AbsoluteY -> printf "$%04X,Y @ %04X = %02X" addressAddr memAddr storedVal
            _ -> ""
  where
    getMemAddrAndStoredValue = case addressing of
        Immediate -> return (0, 0)
        None -> return (0, 0)
        Accumulator -> return (0, 0)
        _ -> do
            addr <- getOperandAddr' addressing
            byte <- withBus $ readByte addr
            return (unAddr addr, unByte byte)

getCPUStateTrace :: CPU r String
getCPUStateTrace = withCPUState $ \st ->
    printf
        "A:%02X X:%02X Y:%02X P:%02X SP:%02X"
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
