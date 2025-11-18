module Nes.APU.Monad (
    APU (..),
    runAPU,
    modifyAPUState,
    modifyAPUStateWithInterrupt,
    withAPUState,
    modifyFilterChain,
    modifyInterruptStatus,
    withInterruptStatus,
) where

import Control.Monad.IO.Class
import Nes.APU.State
import Nes.APU.State.Filter.Chain (FilterChain)
import Nes.Interrupt

newtype APU r a = MkAPU
    { unAPU :: APUState -> InterruptStatus -> (APUState -> InterruptStatus -> a -> IO r) -> IO r
    }
    deriving (Functor)

instance Applicative (APU r) where
    {-# INLINE pure #-}
    pure a = MkAPU $ \(!st) (!interr) cont -> cont st interr a

    {-# INLINE liftA2 #-}
    liftA2 f (MkAPU a) (MkAPU b) = MkAPU $ \(!st) (!interr) cont ->
        a st interr $ \(!st') (!interr') !a' -> b st' interr' $ \(!st'') (!interr'') !b' -> cont st'' interr'' (f a' b')

instance Monad (APU r) where
    {-# INLINE (>>=) #-}
    (MkAPU a) >>= next = MkAPU $ \st interr cont ->
        a st interr $ \(!st') (!interr') (!a') -> unAPU (next a') st' interr' cont

instance MonadIO (APU r) where
    {-# INLINE liftIO #-}
    liftIO io = MkAPU $ \st interr cont -> io >>= cont st interr

instance MonadFail (APU r) where
    {-# INLINE fail #-}
    fail = liftIO . fail

{-# INLINE runAPU #-}
runAPU :: APUState -> InterruptStatus -> APU (a, APUState, InterruptStatus) a -> IO (a, APUState, InterruptStatus)
runAPU !st !s f = unAPU f st s $ \(!st') (!interr) a -> return (a, st', interr)

{-# INLINE modifyAPUState #-}
modifyAPUState :: (APUState -> APUState) -> APU r ()
modifyAPUState f = MkAPU $ \(!st) (!interr) cont -> cont (f st) interr ()

{-# INLINE modifyAPUStateWithInterrupt #-}
modifyAPUStateWithInterrupt :: (APUState -> InterruptStatus -> (APUState, InterruptStatus)) -> APU r ()
modifyAPUStateWithInterrupt f = MkAPU $ \(!st) !interr cont ->
    let (st', interr') = f st interr in cont st' interr' ()

{-# INLINE withAPUState #-}
withAPUState :: (APUState -> a) -> APU r a
withAPUState f = MkAPU $ \(!st) !interr cont -> cont st interr (f st)

{-# INLINE modifyFilterChain #-}
modifyFilterChain :: (FilterChain -> FilterChain) -> APU r ()
modifyFilterChain f = MkAPU $ \(!st) !interr cont ->
    cont st{filterChain = f $ filterChain st} interr ()

{-# INLINE modifyInterruptStatus #-}
modifyInterruptStatus :: (InterruptStatus -> InterruptStatus) -> APU r ()
modifyInterruptStatus f = MkAPU $ \(!st) !interrupt cont -> cont st (f interrupt) ()

{-# INLINE withInterruptStatus #-}
withInterruptStatus :: (InterruptStatus -> a) -> APU r a
withInterruptStatus f = MkAPU $ \(!st) !interrupt cont -> cont st interrupt (f interrupt)
