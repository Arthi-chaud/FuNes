module Nes.Internal.MonadState (
    MonadState (..),
    modify,
    modify',
    gets,
    getsM,
) where

-- | Very similar to mtl's StateMonad, however we don't use functional dependencies to resolve the type of the state. Monads that have multiple instances for this class will have to use explicit type application
class (Monad m) => MonadState s m where
    get :: m s
    set :: s -> m ()

{-# INLINE modify #-}
modify :: (MonadState s m) => (s -> s) -> m ()
modify f = get >>= set . f

{-# INLINE modify' #-}
modify' :: (MonadState s m) => (s -> (a, s)) -> m a
modify' f = do
    (a, s) <- gets f
    set s
    return a

{-# INLINE gets #-}
gets :: (MonadState s m) => (s -> a) -> m a
gets f = fmap f get

{-# INLINE getsM #-}
getsM :: (MonadState s m) => (s -> m a) -> m a
getsM f = get >>= f
