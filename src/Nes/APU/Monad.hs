module Nes.APU.Monad (
    APU (..),
    runAPU,
    modifyAPUState,
    withAPUState,
) where

import Control.Monad.IO.Class
import Nes.APU.State
import Nes.Bus

newtype APU r a = MkAPU
    { unAPU :: APUState -> Bus -> (APUState -> Bus -> a -> IO r) -> IO r
    }
    deriving (Functor)

instance Applicative (APU r) where
    {-# INLINE pure #-}
    pure a = MkAPU $ \st bus cont -> cont st bus a

    {-# INLINE liftA2 #-}
    liftA2 f (MkAPU a) (MkAPU b) = MkAPU $ \st bus cont ->
        a st bus $ \st' bus' a' -> b st' bus' $ \st'' bus'' b' -> cont st'' bus'' (f a' b')

instance Monad (APU r) where
    {-# INLINE (>>=) #-}
    (MkAPU a) >>= next = MkAPU $ \st bus cont ->
        a st bus $ \st' bus' a' -> unAPU (next a') st' bus' cont

instance MonadIO (APU r) where
    {-# INLINE liftIO #-}
    liftIO io = MkAPU $ \st bus cont -> io >>= cont st bus

instance MonadFail (APU r) where
    {-# INLINE fail #-}
    fail = liftIO . fail

{-# INLINE runAPU #-}
runAPU :: APUState -> Bus -> APU (a, APUState) a -> IO (a, APUState)
runAPU st bus f = unAPU op st bus $ \_ _ a -> return a
  where
    op = f >>= \a -> withAPUState (a,)

{-# INLINE modifyAPUState #-}
modifyAPUState :: (APUState -> APUState) -> APU r ()
modifyAPUState f = MkAPU $ \st bus cont -> cont (f st) bus ()

{-# INLINE withAPUState #-}
withAPUState :: (APUState -> a) -> APU r a
withAPUState f = MkAPU $ \st bus cont -> cont st bus (f st)
