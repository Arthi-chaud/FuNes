module Nes.APU.State.Channel (
    -- * Definition
    Channel (..),
    ChannelByte (..),

    -- * Abstraction
    IsChannel (..),

    -- * Use bytes
    setChannelByte,
    withChannelByte,
    getChannelByte,
) where

import Nes.Memory

-- | A 4-byte register for the APU
-- class IChannel
data Channel = MkChannel
    { byte1 :: {-# UNPACK #-} !Byte
    , byte2 :: {-# UNPACK #-} !Byte
    , byte3 :: {-# UNPACK #-} !Byte
    , byte4 :: {-# UNPACK #-} !Byte
    }
    deriving (Eq, Show)

-- | Enum to get a single byte from a 'Channel'
data ChannelByte
    = Byte1
    | Byte2
    | Byte3
    | Byte4
    deriving (Eq, Show, Enum)

-- | Interface for data types that internally have a 'Channel'
class IsChannel a where
    toChannel :: a -> Channel
    fromChannel :: Channel -> a

instance IsChannel Channel where
    toChannel = id
    fromChannel = id

-- | Update a given byte from a 'IsChannel'
setChannelByte :: (IsChannel c) => ChannelByte -> (Byte -> Byte) -> c -> c
setChannelByte n f obj = fromChannel $ case n of
    Byte1 -> chan{byte1 = f $ byte1 chan}
    Byte2 -> chan{byte2 = f $ byte2 chan}
    Byte3 -> chan{byte3 = f $ byte3 chan}
    Byte4 -> chan{byte4 = f $ byte4 chan}
  where
    !chan = toChannel obj

withChannelByte :: (IsChannel c) => ChannelByte -> (Byte -> a) -> c -> a
withChannelByte n f obj = case n of
    Byte1 -> f $ byte1 chan
    Byte2 -> f $ byte2 chan
    Byte3 -> f $ byte3 chan
    Byte4 -> f $ byte4 chan
  where
    chan = toChannel obj

getChannelByte :: (IsChannel c) => ChannelByte -> c -> Byte
getChannelByte n = withChannelByte n id
