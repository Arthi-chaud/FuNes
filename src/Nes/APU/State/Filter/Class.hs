{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nes.APU.State.Filter.Class (Filter (..)) where

import Nes.APU.State.Filter.Constants

class Filter m a where
    consume :: Sample -> a -> m a
    output :: a -> m Sample

instance (Monad m, Filter m a, Filter m b) => Filter m (Either a b) where
    consume sample = either (fmap Left . consume sample) (fmap Right . consume sample)
    output = either (output @m) (output @m)
