module Control.Monad.Trans.Maybe where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype MaybeT m a = MaybeT {
    runMaybeT :: m (Maybe a)
  }

instance Monad m => Functor (MaybeT m) where
  fmap f m = MaybeT (fmap (fmap f) (runMaybeT m))

instance Monad m => Applicative (MaybeT m) where
  pure x = MaybeT (return (Just x))

  mf <*> m = MaybeT $ do f <- runMaybeT mf
                         v <- runMaybeT m
                         return (f <*> v)

instance Monad m => Alternative (MaybeT m) where
  empty = MaybeT (return Nothing)

  x <|> y = MaybeT $ do v <- runMaybeT x
                        case v of
                          Nothing -> runMaybeT y
                          Just _  -> return v

instance Monad m => Monad (MaybeT m) where
  return = pure

  m >>= f = MaybeT $ do v <- runMaybeT m
                        maybe (return Nothing) (runMaybeT . f) v

instance MonadFail m => MonadFail (MaybeT m) where
  fail _ = MaybeT (return Nothing)

instance MonadTrans MaybeT where
  lift m = MaybeT (fmap Just m)

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT
