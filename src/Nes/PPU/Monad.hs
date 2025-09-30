{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nes.PPU.Monad (PPU (..)) where

import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Functor ((<&>))
import Data.Ix
import Nes.Memory
import Nes.Memory.Unsafe ()
import Nes.PPU.Constants
import Nes.PPU.State
import Nes.Rom (Mirroring (..))

newtype PPU r a = MkPPU
    { unPPU ::
        PPUState ->
        ByteString -> -- CHR Rom
        MemoryPointer -> -- Palette Table
        MemoryPointer -> -- VRAM
        MemoryPointer -> -- OAM Data
        ( PPUState ->
          ByteString ->
          MemoryPointer ->
          MemoryPointer ->
          MemoryPointer ->
          a ->
          IO r
        ) -> -- Continuation
        IO r
    }
    deriving (Functor)

instance Applicative (PPU r) where
    pure a = MkPPU $ \st chr plt vram oam cont -> cont st chr plt vram oam a
    liftA2 f (MkPPU a) (MkPPU b) = MkPPU $ \st chr plt vram oam cont ->
        a st chr plt vram oam $ \st' chr' plt' vram' oam' aRes ->
            b st' chr' plt' vram' oam' $ \st'' chr'' plt'' vram'' oam'' bRes ->
                cont st'' chr'' plt'' vram'' oam'' (f aRes bRes)

instance Monad (PPU r) where
    (MkPPU a) >>= next = MkPPU $ \st chr plt vram oam cont ->
        a st chr plt vram oam $ \st' chr' plt' vram' oam' aRes ->
            unPPU (next aRes) st' chr' plt' vram' oam' cont
instance (MonadIO (PPU r)) where
    liftIO io = MkPPU $ \st chr plt vram oam cont ->
        io >>= cont st chr plt vram oam

instance (MonadFail (PPU r)) where
    fail s = liftIO $ fail s

getPaletteTable :: PPU r MemoryPointer
getPaletteTable = MkPPU $ \st chr plt vram oam cont ->
    cont st chr plt vram oam plt

getChrRom :: PPU r ByteString
getChrRom = MkPPU $ \st chr plt vram oam cont ->
    cont st chr plt vram oam chr

getVram :: PPU r MemoryPointer
getVram = MkPPU $ \st chr plt vram oam cont ->
    cont st chr plt vram oam vram

withPPUState :: (PPUState -> a) -> PPU r a
withPPUState f = MkPPU $ \st chr plt vram oam cont ->
    cont st chr plt vram oam (f st)

modifyPPUState :: (PPUState -> PPUState) -> PPU r ()
modifyPPUState f = MkPPU $ \st chr plt vram oam cont ->
    cont (f st) chr plt vram oam ()

writeAddressRegister :: Byte -> PPU r ()
writeAddressRegister byte = modifyPPUState $ \st ->
    st{addressRegister = addressRegisterUpdate byte $ addressRegister st}

writeControlRegister :: Byte -> PPU r ()
writeControlRegister byte = modifyPPUState $ \st ->
    st{controlRegister = byte}

incrementVramAddr :: PPU r ()
incrementVramAddr = modifyPPUState $ \st ->
    st
        { addressRegister =
            addressRegisterIncrement
                (vramAddrIncrement st)
                (addressRegister st)
        }

readData :: PPU r Byte
readData = do
    addr <- withPPUState $ addressRegisterGet . addressRegister
    incrementVramAddr
    go addr
  where
    go addr
        | inRange chrRomRange addr = do
            res <- withPPUState internalBuffer
            value <- Byte <$> (getChrRom <&> (`BS.index` byteToInt res))
            modifyPPUState $ \st -> st{internalBuffer = value}
            return value
        | inRange vramRange addr = do
            res <- withPPUState internalBuffer
            value <- getVram >>= liftIO . readByte (byteToAddr res)
            modifyPPUState $ \st -> st{internalBuffer = value}
            return res
        | inRange unusedAddrRange addr = fail "Address range should not be accessed"
        | addr `elem` paletteIndexes = do
            plt <- getPaletteTable
            liftIO $ readByte (addr - 0x10 - 0x3f00) plt
        | inRange paletteTableRange addr = do
            plt <- getPaletteTable
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
    go addr
  where
    go addr
        | inRange chrRomRange addr = fail "Invalid write to CHR Rom"
        | inRange vramRange addr = do
            vram <- getVram
            mirr <- withPPUState mirroring
            writeByte byte (mirrorVramAddr mirr addr) vram
        | inRange unusedAddrRange addr = fail "Invalid write in address space"
        | addr `elem` paletteIndexes = do
            plt <- getPaletteTable
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

-- TODO Writing to 0x2007 increments addr register

-- TODO It is a memory interface
