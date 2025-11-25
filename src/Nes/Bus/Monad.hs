module Nes.Bus.Monad (Bus (..), runBus, tick, liftPPU, liftAPU, liftController) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Functor (($>))
import Data.Ix
import Foreign
import Nes.APU.BusInterface
import Nes.APU.Monad
import Nes.APU.State
import qualified Nes.APU.Tick as APU
import Nes.Bus.Constants
import Nes.Bus.State
import Nes.Controller
import Nes.FlagRegister (clearFlag)
import Nes.Internal.MonadState
import Nes.Interrupt (InterruptStatus)
import Nes.Memory
import Nes.PPU.Constants (oamDataSize)
import Nes.PPU.Monad hiding (tick)
import qualified Nes.PPU.Monad as PPUM
import Nes.PPU.State hiding (cycles)
import Nes.Rom

newtype Bus r a = MkBus {unBus :: BusState -> (BusState -> a -> IO r) -> IO r} deriving (Functor)

instance Applicative (Bus r) where
    {-# INLINE pure #-}
    pure a = MkBus $ \bus cont -> cont bus a

    {-# INLINE liftA2 #-}
    liftA2 f (MkBus a) (MkBus b) = MkBus $ \bus cont ->
        a bus $ \bus' a' -> b bus' $ \bus'' b' -> cont bus'' (f a' b')

instance Monad (Bus r) where
    {-# INLINE (>>=) #-}
    (MkBus a) >>= next = MkBus $ \bus cont ->
        a bus $ \bus' a' -> unBus (next a') bus' $ \bus'' res -> cont bus'' res

instance MonadIO (Bus r) where
    {-# INLINE liftIO #-}
    liftIO io = MkBus $ \bus cont -> io >>= cont bus

instance MonadFail (Bus r) where
    {-# INLINE fail #-}
    fail = liftIO . fail

instance MonadState BusState (Bus r) where
    {-# INLINE set #-}
    set bus' = MkBus $ \_ cont -> cont bus' ()
    {-# INLINE get #-}
    get = MkBus $ \bus cont -> cont bus bus

{-# INLINE runBus #-}
runBus :: BusState -> Bus (a, BusState) a -> IO (a, BusState)
runBus bus f = unBus f bus (\bus' a -> return (a, bus'))

{-# INLINE liftPPU #-}
liftPPU :: PPU (a, PPUState) a -> Bus r a
liftPPU f = MkBus $ \bus cont -> do
    (res, ppuSt) <- runPPU (_ppuState bus) (_ppuPointers bus) (_cartridge bus) f
    cont (bus{_ppuState = ppuSt}) res

{-# INLINE liftAPU #-}
liftAPU :: APU (a, APUState, InterruptStatus) a -> Bus r a
liftAPU f = MkBus $ \bus cont -> do
    (!res, !apuSt', !cpuInterr') <- runAPU (_apuState bus) (_cpuInterrupt bus) f
    cont (bus{_apuState = apuSt', _cpuInterrupt = cpuInterr'}) res

{-# INLINE liftController #-}
liftController :: Controller (a, ControllerState) a -> Bus r a
liftController f = MkBus $ \bus cont ->
    let
        (res, controller') = runController f (_controller bus)
     in
        cont (bus{_controller = controller'}) res

tick :: Int -> Bus r ()
tick n = MkBus $ \bus cont -> do
    let unsleptCycles_ = n + _unsleptCycles bus
    (newLastSleepTime, newUnsleptCycles) <-
        _cycleCallback bus (_lastSleepTime bus) unsleptCycles_
    (isNewFrame, ppuSt) <- runPPU (_ppuState bus) (_ppuPointers bus) (_cartridge bus) $ do
        before <- use nmiInterrupt
        _ <- PPUM.tick (n * 3)
        after <- use nmiInterrupt
        return (not before && after)
    ((), !apuSt, !interr) <- runAPU (_apuState bus) (_cpuInterrupt bus) $ APU.tick (odd (Nes.Bus.State._cycles bus)) n
    let bus' =
            bus
                { _unsleptCycles = newUnsleptCycles
                , _ppuState = ppuSt
                , _apuState = apuSt
                , _cycles = fromIntegral n + Nes.Bus.State._cycles bus
                , _lastSleepTime = newLastSleepTime
                , _cpuInterrupt = interr
                }
    if isNewFrame
        then do
            _onNewFrame bus' bus'
            controller' <- liftIO (_pollControls bus' $ _controller bus')
            cont bus'{_controller = controller'} ()
        else
            cont bus' ()

data BusReadOutput = OpenBus | DataBus Byte | Internal Byte

instance MemoryInterface () (Bus r) where
    readByte idx () =
        go >>= \case
            DataBus byte -> (dataBus .= byte) $> byte
            OpenBus -> use dataBus
            Internal byte -> return byte
      where
        go
            | inRange ramRange idx = do
                let mirroredDownAddr = idx .&. 0b11111111111 -- 11 bits
                fmap DataBus . liftIO . readByte mirroredDownAddr =<< use cpuVram
            | inRange ppuRegisters idx = do
                let mirroredIdx = Addr . fromIntegral $ addrToInt (idx - fst ppuRegisters) `mod` 8
                    onInvalidRead = DataBus <$> uses ppuState _ioBus
                case mirroredIdx of
                    0 ->
                        if idx == 0x2000
                            then onInvalidRead
                            else
                                let
                                    addr1 = idx .&. 0b0010000000000111
                                 in
                                    DataBus <$> readByte addr1 ()
                    1 -> onInvalidRead
                    2 -> liftPPU $ do
                        st <- readStatus
                        -- https://www.nesdev.org/wiki/PPU_registers#PPUSTATUS_-_Rendering_events_($2002_read)
                        statusRegister %= clearFlag VBlankStarted
                        oldIoBusState <- use ioBus
                        let newIoBus = (st .&. 0b11100000) .|. (oldIoBusState .&. 0b11111)
                        ioBus .= newIoBus
                        return $ DataBus st
                    3 -> onInvalidRead
                    4 -> fmap DataBus $ liftPPU $ do
                        res <- readOamData
                        ioBus .= res
                        return res
                    5 -> onInvalidRead
                    6 -> onInvalidRead
                    7 -> fmap DataBus $ liftPPU $ do
                        res <- readData
                        ioBus .= res
                        return res
                    _ -> error "Cannot happen"
            | inRange prgRomRange idx = do
                rom <- use cartridge
                DataBus <$> readPrgRomAddr (idx - fst prgRomRange) rom readByte
            | idx == 0x4014 = return $ DataBus 0
            | idx == 0x4016 = do
                res <- liftController readButtonStatus
                oldDataBusState <- use dataBus
                let newDataBus = (oldDataBusState .&. 0b11100000) .|. (res .&. 0b11111)
                return $ DataBus newDataBus
            | idx == 0x4017 = do
                oldDataBusState <- use dataBus
                return $ DataBus $ oldDataBusState .&. 0b11100000
            | (0x4000, 0x4017) `inRange` idx = do
                res <- liftAPU $ readFromAPU idx
                case res of
                    Nothing -> return OpenBus
                    Just b -> do
                        b' <- do
                            if idx == 0x4015
                                then do
                                    bit5 <- uses dataBus (`testBit` 5)
                                    return $ if bit5 then b `setBit` 5 else b `clearBit` 5
                                else return b
                        return $ Internal b'
            | otherwise = return OpenBus

    writeByte byte idx () = guardWriteBound idx $ do
        dataBus .= byte
        go
      where
        go
            | inRange ramRange idx =
                let
                    addr = idx .&. 0b11111111111
                 in
                    liftIO . writeByte byte addr =<< use cpuVram
            | inRange ppuRegisters idx = do
                let mirroredIdx = Addr . fromIntegral $ addrToInt (idx - fst ppuRegisters) `mod` 8
                liftPPU $ ioBus .= byte
                case mirroredIdx of
                    0 ->
                        if idx == 0x2000
                            then liftPPU $ writeToControlRegister byte
                            else
                                let
                                    addr = idx .&. 0b0010000000000111
                                 in
                                    writeByte byte addr ()
                    1 -> liftPPU $ maskRegister .= MkMR byte
                    2 -> return ()
                    3 -> liftPPU $ oamOffset .= byte
                    4 -> liftPPU $ writeOamData byte
                    5 -> liftPPU $ setScrollRegister byte
                    6 -> liftPPU $ writeToAddressRegister byte
                    7 -> liftPPU $ writeData byte
                    _ -> error "Cannot happen"
            | inRange prgRomRange idx = liftIO $ putStrLn "Cannot write to catridge"
            | idx == 0x4014 = do
                let high = byteToAddr byte `shiftL` 8
                bytes <- forM [0 .. oamDataSize - 1] $ \i -> do
                    readByte (high + Addr (fromIntegral i)) ()
                liftPPU $ writeListToOam bytes
                cycles_ <- use Nes.Bus.State.cycles
                -- TODO 1) ticks should be done 256 * 2 (as it's a writting operarion) times
                -- TODO 2) Not sure about about the tick count
                tick (513 + fromEnum (odd cycles_))
            | idx == 0x4016 = liftController $ setStrobe byte
            | (0x4000, 0x4017) `inRange` idx = liftAPU $ writeToAPU idx byte
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

{-# INLINE guardWriteBound #-}
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
