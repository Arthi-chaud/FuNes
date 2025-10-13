module Nes.Render (
    render,
) where

import Nes.Bus
import Nes.Render.Background
import Nes.Render.Frame
import Nes.Render.Sprite

render :: Frame -> Bus -> IO ()
render frame bus = do
    fb <- newFrameBuffer
    renderBackground fb bus
    renderSprites fb bus
    renderFrameBuffer fb frame
