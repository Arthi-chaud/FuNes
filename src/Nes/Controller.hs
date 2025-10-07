module Nes.Controller (
    Controller (..),
    newController,
    ControllerButtonStatus (..),
    ControllerButton (..),

    -- * Monad
    ControllerM (..),
    runControllerM,
    setStrobe,
    setButtonAsPressed,
    readButtonStatus,
) where

import Data.Bits
import Nes.FlagRegister
import Nes.Memory (Byte (Byte, unByte))

data Controller = MkC {strobe :: Bool, buttonIdx :: Byte, buttonStatus :: ControllerButtonStatus}

newController :: Controller
newController = MkC False 0 (MkCBS 0)

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

newtype ControllerM r a = MkCM {unCM :: Controller -> (Controller -> a -> r) -> r}

runControllerM :: ControllerM (a, Controller) a -> Controller -> (a, Controller)
runControllerM (MkCM f) controller = f controller (\controller' res -> (res, controller'))

-- | Sets the strobe state if the byte's first bit is set
setStrobe :: Byte -> ControllerM r ()
setStrobe byte = MkCM $ \controller cont ->
    let
        strobe_ = testBit byte 0
        buttonIdx_ = if strobe_ then 0 else buttonIdx controller
     in
        cont (controller{strobe = strobe_, buttonIdx = buttonIdx_}) ()

-- | Returns 1 if the button at the 'buttonIdx' is pressed, or 0 if not.
--
-- Increments the buttonIdx
-- Always returns 1 when the offset if larger than the button count
readButtonStatus :: ControllerM r Byte
readButtonStatus = MkCM $ \controller cont ->
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

setButtonAsPressed :: ControllerButton -> Bool -> ControllerM r ()
setButtonAsPressed status pressed = MkCM $ \controller cont ->
    cont
        controller{buttonStatus = setFlag' status pressed (buttonStatus controller)}
        ()
