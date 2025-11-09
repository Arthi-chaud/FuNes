module Nes.APU.State.LengthCounter (
    LengthCounter (..),
    newLengthCounter,
    tickLengthCounter,
    loadLengthCounter,
    clearAndHaltLengthCounter,

    -- * Class
    HasLengthCounter (..),
    withLengthCounter,
    isSilencedByLengthCounter,
) where

import Data.List ((!?))

data LengthCounter = MkLC {remainingLength :: Int, isHalted :: Bool, tableIndex :: Int}

newLengthCounter :: LengthCounter
newLengthCounter = MkLC 0 False 0

tickLengthCounter :: LengthCounter -> LengthCounter
tickLengthCounter lc =
    if remainingLength lc > 0 && not (isHalted lc)
        then lc{remainingLength = remainingLength lc - 1}
        else lc

clearAndHaltLengthCounter :: LengthCounter -> LengthCounter
clearAndHaltLengthCounter lc = lc{remainingLength = 0, isHalted = True}

-- | Load Length using the argument a an index in the length table
--
-- Note: It must not be done when the enabled bit (4015) is clear
loadLengthCounter :: Int -> LengthCounter -> LengthCounter
loadLengthCounter idx lc = case lengthTable !? idx of
    Just l -> lc{remainingLength = l, tableIndex = idx}
    Nothing -> lc -- Index is invalid

class HasLengthCounter a where
    getLengthCounter :: a -> LengthCounter
    setLengthCounter :: LengthCounter -> a -> a

withLengthCounter :: (HasLengthCounter a) => (LengthCounter -> LengthCounter) -> a -> a
withLengthCounter f a = setLengthCounter (f $ getLengthCounter a) a

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
