{-# LANGUAGE RecordWildCards #-}

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
    ControlRegisterFlag (..),
    unsafeFlagToBitOffset,
    getStatusFlagPure,
    setStatusFlagPure,
    clearStatusFlagPure,
    setStatusFlagPure',
    vramAddrIncrement,

    -- * Constants
    vramSize,
    oamDataSize,
    paletteTableSize,
) where

import Data.Bits
import Nes.Memory (Addr (..), Byte (..), bytesToAddr)
import Nes.Rom

vramSize :: Int
vramSize = 2048

oamDataSize :: Int
oamDataSize = 64 * 4

paletteTableSize :: Int
paletteTableSize = 32

data PPUState = MkPPUState
    { mirroring :: Mirroring
    , controlRegister :: Byte
    , addressRegister :: AddressRegister
    }

newPPUState :: Mirroring -> PPUState
newPPUState mirroring =
    let addressRegister = newAddressRegister
        controlRegister = 0
     in MkPPUState{..}

data AddressRegister = AddressRegister
    { low :: {-# UNPACK #-} !Byte
    , high :: {-# UNPACK #-} !Byte
    , highPtr :: {-# UNPACK #-} !Bool
    }

newAddressRegister :: AddressRegister
newAddressRegister = AddressRegister 0 0 True

-- | Sets an Addr to the AddressRegister's value
addressRegisterSet :: Addr -> AddressRegister -> AddressRegister
addressRegisterSet (Addr bytes) ar = ar{low = low, high = high}
  where
    low = fromIntegral (bytes .&. 0xff)
    high = fromIntegral (bytes `shiftR` 8)

-- | Sets the high or lower byte of the AddressRegister's value,
-- depending on the highPtr state
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
addressRegisterResetLatch :: AddressRegister -> AddressRegister
addressRegisterResetLatch ar = ar{highPtr = True}

-- | Get the AddressRegister's value as an Addr
addressRegisterGet :: AddressRegister -> Addr
addressRegisterGet (AddressRegister low high _) = bytesToAddr low high

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

unsafeFlagToBitOffset :: ControlRegisterFlag -> Int
unsafeFlagToBitOffset = fromEnum

vramAddrIncrement :: PPUState -> Byte
vramAddrIncrement st =
    if testBit (controlRegister st) (fromEnum VramAddIncrement) then 32 else 1

setStatusFlagPure :: ControlRegisterFlag -> PPUState -> PPUState
setStatusFlagPure flag = setStatusFlagPure' flag True

setStatusFlagPure' :: ControlRegisterFlag -> Bool -> PPUState -> PPUState
setStatusFlagPure' flag bool st = st{controlRegister = (if bool then setBit else clearBit) (controlRegister st) (unsafeFlagToBitOffset flag)}

clearStatusFlagPure :: ControlRegisterFlag -> PPUState -> PPUState
clearStatusFlagPure flag = setStatusFlagPure' flag False

getStatusFlagPure :: ControlRegisterFlag -> PPUState -> Bool
getStatusFlagPure flag st = testBit (controlRegister st) (unsafeFlagToBitOffset flag)

-- TODO Remove CR type
