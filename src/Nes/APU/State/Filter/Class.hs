module Nes.APU.State.Filter.Class (Filter (..)) where

import Nes.APU.State.Filter.Constants

class Filter a where
    consume :: Sample -> a -> a
    output :: a -> Sample

instance (Filter a, Filter b) => Filter (Either a b) where
    consume sample = either (Left . consume sample) (Right . consume sample)
    output = either output output
