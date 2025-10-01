{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nes.FlagRegister (FlagRegister (..), getFlag, setFlag, clearFlag, setFlag') where

import Data.Bits
import Data.Kind (Type)
import Nes.Memory

class FlagRegister a where
    type Flag a :: Type
    fromByte :: Byte -> a
    toByte :: a -> Byte
    flagToBitOffset :: Flag a -> Int

setFlag :: (FlagRegister a) => Flag a -> a -> a
setFlag flag = setFlag' flag True

clearFlag :: (FlagRegister a) => Flag a -> a -> a
clearFlag flag = setFlag' flag False

setFlag' :: forall a. (FlagRegister a) => Flag a -> Bool -> a -> a
setFlag' flag bool r = let byte = toByte r in fromByte $ (if bool then setBit else clearBit) byte (flagToBitOffset @a flag)

getFlag :: forall a. (FlagRegister a) => Flag a -> a -> Bool
getFlag f r = testBit (toByte r) (flagToBitOffset @a f)
