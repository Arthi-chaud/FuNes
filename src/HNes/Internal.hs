{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HNes.Internal where

import Data.ByteString
import Data.Maybe
import Data.Word

newtype ProgramCounter = PC {unPC :: Word16} deriving (Eq, Show, Num)

-- | Programs are read-only memory
newtype Program = Program {unProg :: ByteString}

readByteOffProgram :: ProgramCounter -> Program -> Word8
readByteOffProgram (PC offset) (Program bs) = fromJust $ bs !? fromIntegral offset

-- TODO remove fromIntegral
