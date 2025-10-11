module Events (handleEvents) where

import Control.Monad
import Control.Monad.IO.Class
import Nes.Bus.Monad
import Nes.Controller
import SDL
import System.Exit
import Prelude hiding (Either (..))

keymap :: [(Scancode, ControllerButton)]
keymap =
    [ (ScancodeUp, Up)
    , (ScancodeDown, Down)
    , (ScancodeLeft, Left)
    , (ScancodeRight, Right)
    , (ScancodeSpace, Select)
    , (ScancodeReturn, Start)
    , (ScancodeA, A)
    , (ScancodeS, B)
    , (ScancodeZ, B)
    ]

handleEvents :: BusM r ()
handleEvents = do
    events <- liftIO pollEvents
    forM_ events (go . eventPayload)
  where
    exit = liftIO exitSuccess
    go = \case
        QuitEvent -> exit
        KeyboardEvent (KeyboardEventData _ motion _ sym) -> case SDL.keysymScancode sym of
            ScancodeQ -> exit
            ScancodeEscape -> exit
            c -> case lookup c keymap of
                Just b -> withController $ setButtonAsPressed b (motion == Pressed)
                Nothing -> pure ()
        _ -> pure ()
