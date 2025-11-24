{-# LANGUAGE TemplateHaskell #-}

module Nes.PPU.State (
    -- * State
    PPUState (..),
    newPPUState,

    -- *  Address Register
    AddressRegister (highPtr),
    newAddressRegister,
    addressRegisterGet,
    addressRegisterSet,
    addressRegisterIncrement,
    addressRegisterUpdate,
    addressRegisterResetLatch,

    -- * Control Register
    ControlRegister (..),
    ControlRegisterFlag (..),
    getBackgroundPatternAddr,
    getSpritePatternAddr,
    getNametableAddr,

    -- * Status Register
    StatusRegister (..),
    StatusRegisterFlag (..),

    -- * Scroll Register
    ScrollRegister (..),
    newScrollRegister,
    scrollRegisterWrite,
    scrollRegisterResetLatch,

    -- * Mask Register
    MaskRegister (..),
    MaskRegisterFlag (..),

    -- * VRAM
    vramAddrIncrement,

    -- * Lenses
    Nes.PPU.State.mirroring,
    controlRegister,
    addressRegister,
    statusRegister,
    scrollRegister,
    maskRegister,
    internalBuffer,
    oamOffset,
    cycles,
    scanline,
    nmiInterrupt,
    ioBus,
) where

import Control.Lens (makeLenses)
import Data.Bits
import Data.Word
import Nes.FlagRegister
import Nes.Memory (Addr (..), Byte (..), bytesToAddr)
import Nes.Rom

data PPUState = MkPPUState
    { _mirroring :: {-# UNPACK #-} !Mirroring
    , _controlRegister :: {-# UNPACK #-} !ControlRegister
    , _addressRegister :: {-# UNPACK #-} !AddressRegister
    , _statusRegister :: {-# UNPACK #-} !StatusRegister
    , _scrollRegister :: {-# UNPACK #-} !ScrollRegister
    , _maskRegister :: {-# UNPACK #-} !MaskRegister
    , _internalBuffer :: {-# UNPACK #-} !Byte
    , _oamOffset :: {-# UNPACK #-} !Byte
    , _cycles :: {-# UNPACK #-} !Int
    , _scanline :: {-# UNPACK #-} !Word16
    , _nmiInterrupt :: {-# UNPACK #-} !Bool
    , _ioBus :: {-# UNPACK #-} !Byte
    }

newPPUState :: Mirroring -> PPUState
newPPUState _mirroring =
    let _addressRegister = newAddressRegister
        _controlRegister = MkCR 0
        _statusRegister = MkSR 0
        _maskRegister = MkMR 0
        _scrollRegister = newScrollRegister
        _internalBuffer = 0
        _oamOffset = 0
        _cycles = 0
        _scanline = 0
        _nmiInterrupt = False
        _ioBus = 0
     in MkPPUState{..}

data AddressRegister = AddressRegister
    { low :: {-# UNPACK #-} !Byte
    , high :: {-# UNPACK #-} !Byte
    , highPtr :: {-# UNPACK #-} !Bool
    }

newAddressRegister :: AddressRegister
newAddressRegister = AddressRegister 0 0 True

-- | Sets an Addr to the AddressRegister's value
{-# INLINE addressRegisterSet #-}
addressRegisterSet :: Addr -> AddressRegister -> AddressRegister
addressRegisterSet (Addr bytes) ar = ar{low = low, high = high}
  where
    low = fromIntegral (bytes .&. 0xff)
    high = fromIntegral (bytes `shiftR` 8)

-- | Sets the high or lower byte of the AddressRegister's value,
-- depending on the highPtr state
{-# INLINE addressRegisterUpdate #-}
addressRegisterUpdate :: Byte -> AddressRegister -> AddressRegister
addressRegisterUpdate byte ar0 =
    let
        ar1 = if highPtr ar0 then ar0{high = byte} else ar0{low = byte}
        ar2 =
            let
                addr = addressRegisterGet ar1
             in
                if addr > 0x3fff
                    then addressRegisterSet (addr .&. 0x3fff) ar1
                    else ar1
     in
        ar2{highPtr = not $ highPtr ar2}

-- | Increment the AddressRegister's value by the given Byte
{-# INLINE addressRegisterIncrement #-}
addressRegisterIncrement :: Byte -> AddressRegister -> AddressRegister
addressRegisterIncrement byte ar =
    let
        oldLow = low ar
        ar1 = ar{low = low ar + byte}
        ar2 = if oldLow > low ar1 then ar1{high = high ar1 + 1} else ar1
        ar3 = let addr = addressRegisterGet ar2 in if addr > 0x3fff then addressRegisterSet (addr .&. 0x3fff) ar2 else ar2
     in
        ar3

-- | Set highPtr to True
{-# INLINE addressRegisterResetLatch #-}
addressRegisterResetLatch :: AddressRegister -> AddressRegister
addressRegisterResetLatch ar = ar{highPtr = True}

-- | Get the AddressRegister's value as an Addr
{-# INLINE addressRegisterGet #-}
addressRegisterGet :: AddressRegister -> Addr
addressRegisterGet (AddressRegister low high _) = bytesToAddr low high

newtype ControlRegister = MkCR {unCR :: Byte} deriving (Eq, Show)

-- | Flags from the control register
--
-- 7  bit  0
-- ---- ----
-- VPHB SINN
-- |||| ||||
-- |||| ||++- Base nametable address
-- |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
-- |||| |+--- VRAM address increment per CPU read/write of PPUDATA
-- |||| |     (0: add 1, going across; 1: add 32, going down)
-- |||| +---- Sprite pattern table address for 8x8 sprites
-- ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
-- |||+------ Background pattern table address (0: $0000; 1: $1000)
-- ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels)
-- |+-------- PPU master/slave select
-- |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
-- +--------- Generate an NMI at the start of the
--            vertical blanking interval (0: off; 1: on)
--
-- SRC: https://bugzmanov.github.io/nes_ebook/chapter_6_1.html
data ControlRegisterFlag
    = Nametable1
    | Nametable2
    | VramAddIncrement
    | SpritePatternAddr
    | BackgroundPatternAddr
    | SpriteSize
    | MasterSlaveSelect
    | GenerateNMI
    deriving (Eq, Show, Enum)

instance FlagRegister ControlRegister where
    type Flag ControlRegister = ControlRegisterFlag
    fromByte = MkCR
    toByte = unCR
    flagToBitOffset = fromEnum

{-# INLINE vramAddrIncrement #-}
vramAddrIncrement :: ControlRegister -> Byte
vramAddrIncrement st =
    if getFlag VramAddIncrement st then 32 else 1

{-# INLINE getBackgroundPatternAddr #-}
getBackgroundPatternAddr :: ControlRegister -> Addr
getBackgroundPatternAddr st =
    if getFlag BackgroundPatternAddr st
        then 0x1000
        else 0

{-# INLINE getSpritePatternAddr #-}
getSpritePatternAddr :: ControlRegister -> Addr
getSpritePatternAddr st =
    if getFlag SpritePatternAddr st
        then 0x1000
        else 0

{-# INLINE getNametableAddr #-}
getNametableAddr :: ControlRegister -> Addr
getNametableAddr st = case unCR st .&. 0b11 of
    0 -> 0x2000
    1 -> 0x2400
    2 -> 0x2800
    3 -> 0x2c00
    _ -> error "Invalid Nametable Addr"

newtype StatusRegister = MkSR {unSR :: Byte} deriving (Eq, Show)

data StatusRegisterFlag
    = NotUsed1
    | NotUsed2
    | NotUsed3
    | NotUsed4
    | NotUsed5
    | SpriteOverflow
    | SpriteZeroHit
    | VBlankStarted
    deriving (Eq, Show, Enum)

instance FlagRegister StatusRegister where
    type Flag StatusRegister = StatusRegisterFlag
    fromByte = MkSR
    toByte = unSR
    flagToBitOffset = fromEnum

data ScrollRegister = MkScrollR {x :: Byte, y :: Byte, latch :: Bool}

newScrollRegister :: ScrollRegister
newScrollRegister = MkScrollR 0 0 False

{-# INLINE scrollRegisterWrite #-}
scrollRegisterWrite :: Byte -> ScrollRegister -> ScrollRegister
scrollRegisterWrite byte sr =
    let
        sr1 = if not $ latch sr then sr{x = byte} else sr{y = byte}
        sr2 = sr1{latch = not (latch sr1)}
     in
        sr2

{-# INLINE scrollRegisterResetLatch #-}
scrollRegisterResetLatch :: ScrollRegister -> ScrollRegister
scrollRegisterResetLatch st = st{latch = False}

newtype MaskRegister = MkMR {unMR :: Byte} deriving (Eq, Show)

data MaskRegisterFlag
    = GreyScale
    | LeftmostBackground
    | LeftmostSprite
    | ShowBackground
    | ShowSprites
    | EmphRed
    | EmphGree
    | EmphBlue
    deriving (Eq, Show, Enum)

instance FlagRegister MaskRegister where
    type Flag MaskRegister = MaskRegisterFlag
    fromByte = MkMR
    toByte = unMR
    flagToBitOffset = fromEnum

makeLenses ''PPUState
