module Events (handleEvents) where

import Control.Monad
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

handleEvents :: Controller -> IO Controller
handleEvents c = pollEvents >>= foldM (\c' -> go c' . eventPayload) c
  where
    exit = exitSuccess
    go :: Controller -> EventPayload -> IO Controller
    go controller = \case
        QuitEvent -> exit
        KeyboardEvent (KeyboardEventData _ motion _ sym) -> case SDL.keysymScancode sym of
            ScancodeQ -> exit
            ScancodeEscape -> exit
            code -> case lookup code keymap of
                Just b -> return $ snd $ runControllerM (setButtonAsPressed b (motion == Pressed)) controller
                Nothing -> return controller
        _ -> pure controller
