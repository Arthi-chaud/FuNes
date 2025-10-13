module Nes.Render (
    render,
) where

import Nes.Bus
import Nes.Render.Background
import Nes.Render.Frame
import Nes.Render.Sprite

render :: FrameBuffer -> Bus -> IO ()
render fb bus = do
    renderBackground fb bus
    renderSprites fb bus
