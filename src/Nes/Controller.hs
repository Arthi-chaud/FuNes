module Nes.Controller (
    ControllerState (..),
    newControllerState,
    ControllerButtonStatus (..),
    ControllerButton (..),

    -- * Monad
    Controller (..),
    runController,
    setStrobe,
    setButtonAsPressed,
    readButtonStatus,
) where

import Data.Bits
import Nes.FlagRegister
import Nes.Memory (Byte (Byte, unByte))

data ControllerState = MkCS
    { strobe :: {-# UNPACK #-} !Bool
    , buttonIdx :: {-# UNPACK #-} !Byte
    , buttonStatus :: {-# UNPACK #-} !ControllerButtonStatus
    }

newControllerState :: ControllerState
newControllerState = MkCS False 0 (MkCBS 0)

newtype ControllerButtonStatus = MkCBS {unStatus :: Byte}

data ControllerButton
    = A
    | B
    | Select
    | Start
    | Up
    | Down
    | Left
    | Right
    deriving (Eq, Show, Enum)

instance FlagRegister ControllerButtonStatus where
    type Flag ControllerButtonStatus = ControllerButton
    fromByte = MkCBS
    toByte = unStatus
    flagToBitOffset = fromEnum

newtype Controller r a = MkC {unC :: ControllerState -> (ControllerState -> a -> r) -> r}

runController :: Controller (a, ControllerState) a -> ControllerState -> (a, ControllerState)
runController (MkC f) controller = f controller (\controller' res -> (res, controller'))

-- | Sets the strobe state if the byte's first bit is set
setStrobe :: Byte -> Controller r ()
setStrobe byte = MkC $ \controller cont ->
    let
        strobe_ = testBit byte 0
        buttonIdx_ = if strobe_ then 0 else buttonIdx controller
     in
        cont (controller{strobe = strobe_, buttonIdx = buttonIdx_}) ()

-- | Returns 1 if the button at the 'buttonIdx' is pressed, or 0 if not.
--
-- Increments the buttonIdx
-- Always returns 1 when the offset if larger than the button count
readButtonStatus :: Controller r Byte
readButtonStatus = MkC $ \controller cont ->
    if buttonIdx controller > 7
        then cont controller $ Byte 1
        else
            let
                res =
                    boolToByte $
                        testBit
                            (unStatus $ buttonStatus controller)
                            (fromIntegral . unByte $ buttonIdx controller)
                buttonIdx_ = buttonIdx controller + boolToByte (not $ strobe controller)
             in
                cont (controller{buttonIdx = buttonIdx_}) res
  where
    boolToByte = Byte . fromIntegral . fromEnum

setButtonAsPressed :: ControllerButton -> Bool -> Controller r ()
setButtonAsPressed status pressed = MkC $ \controller cont ->
    cont
        controller{buttonStatus = setFlag' status pressed (buttonStatus controller)}
        ()
