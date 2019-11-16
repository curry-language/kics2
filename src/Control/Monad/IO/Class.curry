module Control.Monad.IO.Class (
    MonadIO(..)
  ) where

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO = id
