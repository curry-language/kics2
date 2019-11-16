module Control.Monad.Trans.Cont where

import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

evalContT :: (Monad m) => ContT r m r -> m r
evalContT m = runContT m return

mapContT :: (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f m = ContT $ f . runContT m

withContT :: ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f m = ContT $ runContT m . f

instance Functor (ContT r m) where
  fmap f m = ContT $ \ c -> runContT m (c . f)

instance Applicative (ContT r m) where
  pure x  = ContT ($ x)

  f <*> v = ContT $ \ c -> runContT f $ \ g -> runContT v (c . g)

instance Monad (ContT r m) where
  return x = ContT ($ x)

  m >>= k  = ContT $ \ c -> runContT m (\ x -> runContT (k x) c)

instance MonadFail m => MonadFail (ContT r m) where
  fail msg = ContT $ \ _ -> fail msg

instance MonadTrans (ContT r) where
  lift m = ContT (m >>=)

instance (MonadIO m) => MonadIO (ContT r m) where
  liftIO = lift . liftIO

resetT :: Monad m => ContT r m r -> ContT r' m r
resetT = lift . evalContT

shiftT :: Monad m => ((a -> m r) -> ContT r m r) -> ContT r m a
shiftT f = ContT (evalContT . f)

liftLocal :: Monad m => m r' -> ((r' -> r') -> m r -> m r) ->
    (r' -> r') -> ContT r m a -> ContT r m a
liftLocal ask local f m = ContT $ \ c -> do
    r <- ask
    local f (runContT m (local (const r) . c))

type Cont r = ContT r Identity

cont :: ((a -> r) -> r) -> Cont r a
cont f = ContT (\ c -> Identity (f (runIdentity . c)))

runCont :: Cont r a -> (a -> r) -> r
runCont m k = runIdentity (runContT m (Identity . k))

evalCont :: Cont r r -> r
evalCont m = runIdentity (evalContT m)

mapCont :: (r -> r) -> Cont r a -> Cont r a
mapCont f = mapContT (Identity . f . runIdentity)

withCont :: ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont f = withContT ((Identity .) . f . (runIdentity .))

reset :: Cont r r -> Cont r' r
reset = resetT

shift :: ((a -> r) -> Cont r r) -> Cont r a
shift f = shiftT (f . (runIdentity .))
