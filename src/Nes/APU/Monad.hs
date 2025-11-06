module Nes.APU.Monad (
    APU (..),
    modifyAPUState,
    withAPUState,
    tick,
) where

import Control.Monad.IO.Class
import Nes.APU.State

newtype APU r a = MkAPU {unAPU :: APUState -> (APUState -> a -> IO r) -> IO r} deriving (Functor)

instance Applicative (APU r) where
    {-# INLINE pure #-}
    pure a = MkAPU $ \st cont -> cont st a

    {-# INLINE liftA2 #-}
    liftA2 f (MkAPU a) (MkAPU b) = MkAPU $ \st cont ->
        a st $ \st' a' -> b st' $ \st'' b' -> cont st'' (f a' b')

instance Monad (APU r) where
    {-# INLINE (>>=) #-}
    (MkAPU a) >>= next = MkAPU $ \st cont ->
        a st $ \st' a' -> unAPU (next a') st' $ \st'' res -> cont st'' res

instance MonadIO (APU r) where
    {-# INLINE liftIO #-}
    liftIO io = MkAPU $ \st cont -> io >>= cont st

instance MonadFail (APU r) where
    {-# INLINE fail #-}
    fail = liftIO . fail

{-# INLINE modifyAPUState #-}
modifyAPUState :: (APUState -> APUState) -> APU r ()
modifyAPUState f = MkAPU $ \st cont -> cont (f st) ()

{-# INLINE withAPUState #-}
withAPUState :: (APUState -> a) -> APU r a
withAPUState f = MkAPU $ \st cont -> cont st (f st)

tick :: Int -> APU r ()
tick n = return ()

-- TODO Update Status register when 'a sound has finished playing'
-- TODO Start playing if status register bit is set to one
