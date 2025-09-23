{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Nes.Rom (
    -- * Data type
    Rom,
    prgRom,
    chrRom,
    mapper,
    mirroring,

    -- * Parsing a Rom
    fromFile,
    fromByteString,
    RomParsingError,

    -- * Internal
    unsafeEmptyRom,
) where

import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Nes.Memory (Byte (Byte))

data Mirroring = Vertical | Horizontal | FourScreen deriving (Eq, Show)

data Rom = Rom
    { prgRom :: ByteString
    -- ^ The portion of the ROM that's connected to the CPU
    , chrRom :: ByteString
    -- ^ The portion of the ROM that's connected to the PPU
    , mapper :: Byte
    , mirroring :: Mirroring
    }

type RomParsingError = String

fromFile :: FilePath -> IO (Either RomParsingError Rom)
fromFile = fmap fromByteString . BS.readFile

fromByteString :: ByteString -> (Either RomParsingError Rom)
fromByteString bs = do
    guardHeader
    guardINesVersion
    prgRomSize <- (* prgRomPageSize) . fromIntegral <$> getByte 4
    chrRomSize <- (* chrRomPageSize) . fromIntegral <$> getByte 5
    mapper <- Byte <$> getMapper
    mirroring <- getScreenMirroring
    (prgRomStart, chrRomStart) <- getRomsStarts prgRomSize
    let prgRom = BS.take prgRomSize $ BS.drop prgRomStart bs
        chrRom = BS.drop chrRomSize $ BS.drop chrRomStart bs
    return $ Rom{..}
  where
    guardHeader =
        let header = BS.unpack $ BS.take 4 bs
         in when (header /= nesHeader) $
                Left "File is not in iNES file format"
    guardINesVersion = do
        b7 <- getByte 7
        let inesVersion = (b7 `shiftR` 2) .&. 0b11
        when (inesVersion /= 0) $ Left "iNES 2.0 file format is not supported"
    getMapper = do
        b7 <- getByte 7
        b6 <- getByte 6
        return $ b7 .&. 0b11110000 .|. (b6 `shiftR` 4)
    getScreenMirroring = do
        b6 <- getByte 6
        let isFourScreen = testBit b6 3
            isVerticalMirroring = testBit b6 0
        return $ case (isFourScreen, isVerticalMirroring) of
            (True, _) -> FourScreen
            (False, True) -> Vertical
            (False, False) -> Horizontal
    getRomsStarts prgRomSize = do
        b6 <- getByte 6
        let skipTrainer = testBit b6 2
            prgRomStart = 16 + (if skipTrainer then 512 else 0)
            chrRomStart = prgRomStart + prgRomSize
        return (prgRomStart, chrRomStart)
    getByte n = case BS.indexMaybe bs n of
        Nothing -> Left "Truncated file"
        Just b -> return b

-- | For testing purposes, do not use
unsafeEmptyRom :: Rom
unsafeEmptyRom = Rom BS.empty BS.empty 0 Horizontal

-- TODO Handle iNES2.0

-- Constants

nesHeader :: [Word8]
nesHeader = [0x4e, 0x45, 0x53, 0x1a]

prgRomPageSize :: Int
prgRomPageSize = 16384

chrRomPageSize :: Int
chrRomPageSize = 8192
