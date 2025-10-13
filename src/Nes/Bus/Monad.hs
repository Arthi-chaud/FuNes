{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nes.Bus.Monad (BusM (..), runBusM, tick, withBus, withPPU, withController) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Ix
import Foreign
import Nes.Bus
import Nes.Bus.Constants
import Nes.Controller
import Nes.Memory
import Nes.PPU.Constants (oamDataSize)
import Nes.PPU.Monad hiding (tick)
import qualified Nes.PPU.Monad as PPUM
import Nes.PPU.State
import Nes.Rom

newtype BusM r a = MkBusM {unBusM :: Bus -> (Bus -> a -> IO r) -> IO r} deriving (Functor)

instance Applicative (BusM r) where
    pure a = MkBusM $ \bus cont -> cont bus a
    liftA2 f (MkBusM a) (MkBusM b) = MkBusM $ \bus cont ->
        a bus $ \bus' a' -> b bus' $ \bus'' b' -> cont bus'' (f a' b')

instance Monad (BusM r) where
    (MkBusM a) >>= next = MkBusM $ \bus cont ->
        a bus $ \bus' a' -> unBusM (next a') bus' $ \bus'' res -> cont bus'' res

instance MonadIO (BusM r) where
    liftIO io = MkBusM $ \bus cont -> io >>= cont bus

instance MonadFail (BusM r) where
    fail = liftIO . fail

runBusM :: Bus -> BusM (a, Bus) a -> IO (a, Bus)
runBusM bus f = unBusM f bus (\bus' a -> return (a, bus'))

withBus :: (Bus -> a) -> BusM r a
withBus f = MkBusM $ \bus cont -> cont bus (f bus)

withPPU :: PPU (a, PPUState) a -> BusM r a
withPPU f = MkBusM $ \bus cont -> do
    (res, ppuSt) <- runPPU (ppuState bus) (ppuPointers bus) (cartridge bus) f
    cont (bus{ppuState = ppuSt}) res

withController :: ControllerM (a, Controller) a -> BusM r a
withController f = MkBusM $ \bus cont ->
    let
        (res, controller') = runControllerM f (controller bus)
     in
        cont (bus{controller = controller'}) res

tick :: Int -> BusM r ()
tick n = MkBusM $ \bus cont -> do
    replicateM_ n (cycleCallback bus n)
    ((_isNewFrame, nmiBefore, nmiAfter), ppuSt) <- runPPU (ppuState bus) (ppuPointers bus) (cartridge bus) $ do
        before <- withPPUState nmiInterrupt
        isNewFrame <- PPUM.tick (n * 3)
        after <- withPPUState nmiInterrupt
        return (isNewFrame, before, after)

    let bus' = bus{Nes.Bus.cycles = Nes.Bus.cycles bus + fromIntegral n, ppuState = ppuSt}
    if not nmiBefore && nmiAfter
        then onNewFrame bus' bus' >>= flip cont ()
        else
            cont bus' ()

instance MemoryInterface () (BusM r) where
    readByte idx () = guardReadBound idx go
      where
        go
            | inRange ramRange idx = do
                let mirroredDownAddr = idx .&. 0b11111111111 -- 11 bits
                liftIO . readByte mirroredDownAddr =<< withBus cpuVram
            | inRange ppuRegisters idx = do
                let mirroredIdx = Addr . fromIntegral $ addrToInt (idx - fst ppuRegisters) `mod` 8
                    onInvalidRead = return 0
                case mirroredIdx of
                    0 ->
                        if idx == 0x2000
                            then onInvalidRead
                            else
                                let
                                    addr1 = idx .&. 0b0010000000000111
                                 in
                                    readByte addr1 ()
                    1 -> onInvalidRead
                    2 -> withPPU readStatus
                    3 -> onInvalidRead
                    4 -> withPPU readOamData
                    5 -> onInvalidRead
                    6 -> onInvalidRead
                    7 -> withPPU readData
                    _ -> error "Cannot happen"
            | inRange prgRomRange idx = do
                rom <- withBus cartridge
                readPrgRomAddr (idx - fst prgRomRange) rom readByte
            | idx == 0x4014 = return 0
            | idx == 0x4016 = withController readButtonStatus
            | idx == 0x4017 = return 0 -- Second joypad, ignore
            | otherwise = do
                -- liftIO $ printf "Invalid read at %4x\n" $ unAddr idx
                return 0
    writeByte byte idx () = guardWriteBound idx go
      where
        go
            | inRange ramRange idx =
                let
                    addr = idx .&. 0b11111111111
                 in
                    liftIO . writeByte byte addr =<< withBus cpuVram
            | inRange ppuRegisters idx = do
                let mirroredIdx = Addr . fromIntegral $ addrToInt (idx - fst ppuRegisters) `mod` 8
                case mirroredIdx of
                    0 ->
                        if idx == 0x2000
                            then withPPU $ writeToControlRegister byte
                            else
                                let
                                    addr = idx .&. 0b0010000000000111
                                 in
                                    writeByte byte addr ()
                    1 -> withPPU $ setMaskRegister byte
                    2 -> do
                        liftIO $ putStrLn "Invalid write to PPU status register"
                        return ()
                    3 -> withPPU $ setOamOffset byte
                    4 -> withPPU $ writeOamData byte
                    5 -> withPPU $ setScrollRegister byte
                    6 -> withPPU $ writeToAddressRegister byte
                    7 -> withPPU $ writeData byte
                    _ -> error "Cannot happen"
            | inRange prgRomRange idx = liftIO $ putStrLn "Cannot write to catridge"
            | idx == 0x4014 = do
                let high = byteToAddr byte `shiftL` 8
                bytes <- forM [0 .. oamDataSize - 1] $ \i -> do
                    readByte (high + Addr (fromIntegral i)) ()
                withPPU $ writeListToOam bytes
                cycles_ <- withBus Nes.Bus.cycles
                -- TODO 1) ticks should be done 256 * 2 (as it's a writting operarion) times
                -- TODO 2) Not sure about about the tick count
                tick (513 + fromEnum (odd cycles_))
            | idx == 0x4016 = withController $ setStrobe byte
            | idx == 0x4017 = pure () -- Second joypad, ignore
            | otherwise = pure () -- liftIO $ printf "Ignoring write at %4x\n" $ unAddr idx
    readAddr idx () = do
        low <- readByte idx ()
        high <- readByte (idx + 1) ()
        return $ bytesToAddr low high

    writeAddr addr idx () = do
        let low = unsafeAddrToByte (addr .&. 0xff)
            high = unsafeAddrToByte (addr `shiftR` 8)
        writeByte low idx ()
        writeByte high (idx + 1) ()

guardReadBound :: (MonadFail m) => Addr -> m Byte -> m Byte
guardReadBound idx cont = if idx >= memorySize then return 0 else cont

guardWriteBound :: (MonadFail m) => Addr -> m () -> m ()
guardWriteBound idx = when (idx < memorySize)

-- | The continuation will be called with the translated addr to use on the PRG Rom
-- No bound check are necessary
readPrgRomAddr :: (MonadFail m) => Addr -> Rom -> (Addr -> ForeignPtr Word8 -> m a) -> m a
readPrgRomAddr addr rom cont = do
    let prgRomSize = BS.length (prgRom rom)
        translatedAddr =
            if prgRomSize == 0x4000 && addr >= 0x4000
                then Addr $ unAddr addr `mod` 0x4000
                else addr
    when (addrToInt translatedAddr > prgRomSize) $ fail "Out-of-bound access in ROM"
    let ptr = let (BS.BS ptr' _) = prgRom rom in ptr'
    cont translatedAddr ptr
