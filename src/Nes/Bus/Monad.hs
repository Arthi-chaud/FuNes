{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nes.Bus.Monad (BusM (..), runBusM, tick, withBus, withPPU) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Ix
import Foreign
import Nes.Bus
import Nes.Bus.Constants
import Nes.Memory
import Nes.PPU.Monad hiding (tick)
import qualified Nes.PPU.Monad as PPUM
import Nes.PPU.State
import Nes.Rom
import Text.Printf

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
    (res, ppuSt) <- runPPU (ppuState bus) (ppuPointers bus) f
    cont (bus{ppuState = ppuSt}) res

tick :: Int -> BusM r ()
tick n = MkBusM $ \bus cont -> do
    replicateM_ n (cycleCallback bus)

    ((_isNewFrame, nmiBefore, nmiAfter), ppuSt) <- runPPU (ppuState bus) (ppuPointers bus) $ do
        before <- withPPUState nmiInterrupt
        isNewFrame <- PPUM.tick (n * 3)
        after <- withPPUState nmiInterrupt
        return (isNewFrame, before, after)

    let bus' = bus{Nes.Bus.cycles = Nes.Bus.cycles bus + fromIntegral n, ppuState = ppuSt}
    when (not nmiBefore && nmiAfter) $ onNewFrame bus' bus'
    cont bus' ()

instance MemoryInterface () (BusM r) where
    readByte idx () = checkBound idx >> go
      where
        go
            | inRange ramRange idx =
                liftIO . readByte idx =<< withBus cpuVram
            | idx `elem` [0x2000, 0x2001, 0x2003, 0x2005, 0x2006, 0x4014] =
                fail $
                    printf "Invalid read from write-only PPU address %4x\n" $
                        unAddr idx
            | idx == 0x2002 = withPPU readStatus
            | idx == 0x2004 = withPPU readOamData
            | idx == 0x2007 = withPPU readData
            | inRange (0x2008, snd ramRange) idx =
                let
                    addr1 = idx .&. 0b0010000000000111
                 in
                    readByte addr1 ()
            | inRange prgRomRange idx = do
                rom <- withBus cartridge
                readPrgRomAddr idx rom readByte
            | otherwise = do
                liftIO $ printf "Invalid read at %4x\n" $ unAddr idx
                return 0
    writeByte byte idx () = checkBound idx >> go
      where
        go
            | inRange ramRange idx =
                let
                    addr = idx .&. 0b11111111111
                 in
                    liftIO . writeByte byte addr =<< withBus cpuVram
            | idx == 0x2000 = withPPU $ writeToControlRegister byte
            | idx == 0x2001 = withPPU $ setMaskRegister byte
            | idx == 0x2002 = fail "Invalid write to PPU status register"
            | idx == 0x2003 = withPPU $ setOamOffset byte
            | idx == 0x2004 = withPPU $ writeOamData byte
            | idx == 0x2005 = withPPU $ setScrollRegister byte
            | idx == 0x2006 = withPPU $ writeToAddressRegister byte
            | idx == 0x2007 = withPPU $ writeData byte
            | inRange (0x2008, snd ramRange) idx =
                let
                    addr = idx .&. 0b0010000000000111
                 in
                    writeByte byte addr ()
            | inRange prgRomRange idx = fail "Cannot write to catridge"
            | otherwise = liftIO $ printf "Ignoring write at %4x\n" $ unAddr idx
    readAddr idx () = do
        low <- readByte idx ()
        high <- readByte (idx + 1) ()
        return $ bytesToAddr low high

    writeAddr addr idx () = do
        let low = unsafeAddrToByte (addr .&. 0xff)
            high = unsafeAddrToByte (addr `shiftR` 8)
        writeByte low idx ()
        writeByte high (idx + 1) ()

checkBound :: (MonadFail m) => Addr -> m ()
checkBound idx = when (idx >= memorySize) $ fail "Out-of-bounds memory access"

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
