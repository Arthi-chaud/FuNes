{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nes.PPU.Monad (
    -- * Monad
    PPU (..),
    runPPU,

    -- * Ticks
    tick,

    -- * State
    withPointers,
    withPPUState,
    modifyPPUState,

    -- * Vram
    readData,
    writeData,
    mirrorVramAddr,
    incrementVramAddr,

    -- * Registers
    writeToAddressRegister,
    writeToControlRegister,
    setMaskRegister,
    setOamOffset,
    setScrollRegister,

    -- * Status
    readStatus,

    -- * OAM
    readOamData,
    writeOamData,
    writeListToOam,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import qualified Data.ByteString as BS
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import Data.Ix
import Nes.FlagRegister
import Nes.Memory
import Nes.Memory.Unsafe ()
import Nes.PPU.Constants
import Nes.PPU.Pointers
import Nes.PPU.State
import Nes.Rom (Mirroring (..))

newtype PPU r a = MkPPU
    { unPPU ::
        PPUState ->
        PPUPointers ->
        ( PPUState ->
          PPUPointers ->
          a ->
          IO r
        ) -> -- Continuation
        IO r
    }
    deriving (Functor)

runPPU :: PPUState -> PPUPointers -> PPU (a, PPUState) a -> IO (a, PPUState)
runPPU st ptrs f = unPPU op st ptrs $ \_ _ a -> return a
  where
    op = f >>= \a -> withPPUState (a,)

instance Applicative (PPU r) where
    pure a = MkPPU $ \st ptr cont -> cont st ptr a
    liftA2 f (MkPPU a) (MkPPU b) = MkPPU $ \st ptr cont ->
        a st ptr $ \st' ptr' aRes ->
            b st' ptr' $ \st'' ptr'' bRes ->
                cont st'' ptr'' (f aRes bRes)

instance Monad (PPU r) where
    (MkPPU a) >>= next = MkPPU $ \st ptr cont ->
        a st ptr $ \st' ptr' aRes ->
            unPPU (next aRes) st' ptr' cont
instance (MonadIO (PPU r)) where
    liftIO io = MkPPU $ \st ptr cont ->
        io >>= cont st ptr

instance (MonadFail (PPU r)) where
    fail s = liftIO $ fail s

tick :: Int -> PPU r Bool
tick n = MkPPU $ \st0 ptrs cont -> do
    let st1 = st0{cycles = cycles st0 + fromIntegral n}
    if cycles st1 < 341
        then
            cont st1 ptrs False
        else do
            let st2 = st1{cycles = cycles st1 - 341, scanline = scanline st1 + 1}
                st3 =
                    if scanline st2 == 241
                        then
                            let
                                st' = modifyStatusRegister (setFlag VBlankStarted . clearFlag SpriteZeroHit) st2
                             in
                                if getFlag GenerateNMI (controlRegister st')
                                    then st'{nmiInterrupt = True}
                                    else st'
                        else st2
             in if scanline st3 >= 262
                    then
                        let
                            st4 =
                                modifyStatusRegister
                                    (clearFlag SpriteZeroHit . clearFlag VBlankStarted)
                                    st2{scanline = 0, nmiInterrupt = False}
                         in
                            cont st4 ptrs True
                    else cont st3 ptrs False

withPointers :: (PPUPointers -> a) -> PPU r a
withPointers f = MkPPU $ \st ptr cont ->
    cont st ptr (f ptr)

withPPUState :: (PPUState -> a) -> PPU r a
withPPUState f = MkPPU $ \st ptr cont ->
    cont st ptr (f st)

modifyPPUState :: (PPUState -> PPUState) -> PPU r ()
modifyPPUState f = MkPPU $ \st ptr cont ->
    cont (f st) ptr ()

incrementVramAddr :: PPU r ()
incrementVramAddr = modifyPPUState $ \st ->
    st
        { addressRegister =
            addressRegisterIncrement
                (vramAddrIncrement $ controlRegister st)
                (addressRegister st)
        }

readStatus :: PPU r Byte
readStatus = do
    byte <- unSR <$> withPPUState statusRegister
    modifyPPUState $
        modifyStatusRegister (clearFlag VBlankStarted)
            . modifyAddressRegister addressRegisterResetLatch
            . modifyScrollRegister scrollRegisterResetLatch
    return byte

readOamData :: PPU r Byte
readOamData = do
    oam <- withPointers oamData
    addr <- withPPUState oamOffset
    readByte (byteToAddr addr) oam

writeOamData :: Byte -> PPU r ()
writeOamData byte = do
    oam <- withPointers oamData
    addr <- withPPUState oamOffset
    writeByte byte (byteToAddr addr) oam
    setOamOffset (addr + 1)

writeListToOam :: [Byte] -> PPU r ()
writeListToOam = foldlM (\_ item -> writeOamData item) ()

writeToAddressRegister :: Byte -> PPU r ()
writeToAddressRegister byte =
    modifyPPUState $
        \st -> st{addressRegister = addressRegisterUpdate byte (addressRegister st)}

writeToControlRegister :: Byte -> PPU r ()
writeToControlRegister byte = do
    oldNmi <- withPPUState $ getFlag GenerateNMI . controlRegister
    let newCR = MkCR byte
        newNmi = getFlag GenerateNMI newCR
    modifyPPUState $ \st -> st{controlRegister = newCR}
    isInVBlank <- withPPUState $ getFlag VBlankStarted . statusRegister
    when (not oldNmi && newNmi && isInVBlank) $
        modifyPPUState $
            \st -> st{nmiInterrupt = True}

setMaskRegister :: Byte -> PPU r ()
setMaskRegister byte = modifyPPUState $ \st -> st{maskRegister = MkMR byte}

setOamOffset :: Byte -> PPU r ()
setOamOffset byte = modifyPPUState $ \st -> st{oamOffset = byte}

setScrollRegister :: Byte -> PPU r ()
setScrollRegister byte =
    modifyPPUState $
        modifyScrollRegister $
            scrollRegisterWrite byte

readData :: PPU r Byte
readData = do
    addr <- withPPUState $ addressRegisterGet . addressRegister
    incrementVramAddr
    go addr
  where
    go addr
        | inRange chrRomRange addr = do
            res <- withPPUState internalBuffer
            value <- Byte <$> (withPointers chrRom <&> (`BS.index` byteToInt res))
            modifyPPUState $ \st -> st{internalBuffer = value}
            return value
        | inRange vramRange addr = do
            res <- withPPUState internalBuffer
            mirr <- withPPUState mirroring
            value <- readByte (mirrorVramAddr mirr addr) =<< withPointers vram
            modifyPPUState $ \st -> st{internalBuffer = value}
            return res
        | inRange unusedAddrRange addr = fail "Address range should not be accessed"
        | inRange paletteTableRange addr = do
            plt <- withPointers paletteTable
            -- https://github.com/bugzmanov/nes_ebook/blob/785b9ed8b803d9f4bd51274f4d0c68c14a1b3a8b/code/ch6.1/src/ppu/mod.rs#L169
            let addr1 =
                    if addr `elem` paletteIndexes
                        then addr - 0x10
                        else addr
            liftIO $ readByte (addr1 - 0x3f00) plt
        | otherwise = fail "Unexpected access to mirrored space"

writeData :: Byte -> PPU r ()
writeData byte = do
    addr <- withPPUState $ addressRegisterGet . addressRegister
    incrementVramAddr
    go addr
  where
    go addr
        | inRange chrRomRange addr = fail "Invalid write to CHR Rom"
        | inRange vramRange addr = do
            mirr <- withPPUState mirroring
            writeByte byte (mirrorVramAddr mirr addr) =<< withPointers vram
        | inRange unusedAddrRange addr = fail "Invalid write in address space"
        | inRange paletteTableRange addr = do
            plt <- withPointers paletteTable
            let addr1 =
                    if addr `elem` paletteIndexes
                        then addr - 0x10
                        else addr
            liftIO $ writeByte byte (addr1 - 0x3f00) plt
        | otherwise = fail "Unexpected access to mirrored space"

mirrorVramAddr :: Mirroring -> Addr -> Addr
mirrorVramAddr mirr addr = case (mirr, nameTable) of
    (Vertical, 2) -> vramIndex - 0x800
    (Vertical, 3) -> vramIndex - 0x800
    (Horizontal, 2) -> vramIndex - 0x400
    (Horizontal, 1) -> vramIndex - 0x400
    (Horizontal, 3) -> vramIndex - 0x800
    _ -> vramIndex
  where
    mirroredVram = addr .&. 0b10111111111111
    vramIndex = mirroredVram - 0x2000
    nameTable = unAddr vramIndex `div` 0x400

-- TODO It is a memory interface
