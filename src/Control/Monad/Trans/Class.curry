module Control.Monad.Trans.Class where

class MonadTrans t where
  lift :: Monad m => m a -> t m a
