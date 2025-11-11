module Nes.APU.State.LengthCounter (
    LengthCounter (..),
    newLengthCounter,
    tickLengthCounter,
    loadLengthCounter,
    clearAndHaltLengthCounter,
    enableLengthCounter,
    disableLengthCounter,

    -- * Class
    HasLengthCounter (..),
    withLengthCounter,
    isSilencedByLengthCounter,
) where

import Data.List ((!?))

data LengthCounter = MkLC
    { remainingLength :: {-# UNPACK #-} !Int
    , isHalted :: {-# UNPACK #-} !Bool
    , isEnabled :: {-# UNPACK #-} !Bool
    }

newLengthCounter :: LengthCounter
newLengthCounter = MkLC 0 False False

tickLengthCounter :: LengthCounter -> LengthCounter
tickLengthCounter lc =
    if remainingLength lc > 0 && not (isHalted lc)
        then lc{remainingLength = remainingLength lc - 1}
        else lc

{-# INLINE clearAndHaltLengthCounter #-}
clearAndHaltLengthCounter :: LengthCounter -> LengthCounter
clearAndHaltLengthCounter lc = lc{remainingLength = 0, isHalted = True}

-- | Load Length using the argument a an index in the length table
--
-- Note: It must not be done when the enabled bit (4015) is clear
loadLengthCounter :: Int -> LengthCounter -> LengthCounter
loadLengthCounter idx lc
    | not $ isEnabled lc = lc
    | otherwise = case lengthTable !? idx of
        Just l -> lc{remainingLength = l}
        Nothing -> lc -- Index is invalid

disableLengthCounter :: LengthCounter -> LengthCounter
disableLengthCounter lc = lc{isEnabled = False, remainingLength = 0}

enableLengthCounter :: LengthCounter -> LengthCounter
enableLengthCounter lc = lc{isEnabled = True}

class HasLengthCounter a where
    getLengthCounter :: a -> LengthCounter
    setLengthCounter :: LengthCounter -> a -> a

{-# INLINE withLengthCounter #-}
withLengthCounter :: (HasLengthCounter a) => (LengthCounter -> LengthCounter) -> a -> a
withLengthCounter f a = setLengthCounter (f $ getLengthCounter a) a

{-# INLINE isSilencedByLengthCounter #-}
isSilencedByLengthCounter :: (HasLengthCounter a) => a -> Bool
isSilencedByLengthCounter = (== 0) . remainingLength . getLengthCounter

lengthTable :: [Int]
lengthTable =
    [ 10
    , 254
    , 20
    , 2
    , 40
    , 4
    , 80
    , 6
    , 160
    , 8
    , 60
    , 10
    , 14
    , 12
    , 26
    , 14
    , 12
    , 16
    , 24
    , 18
    , 48
    , 20
    , 96
    , 22
    , 192
    , 24
    , 72
    , 26
    , 16
    , 28
    , 32
    , 30
    ]
