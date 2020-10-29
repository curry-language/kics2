module Control.Monad.Trans.Except where

import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype ExceptT e m a = ExceptT {
    runExceptT :: m (Either e a)
  }

instance Monad m => Functor (ExceptT e m) where
  fmap f = ExceptT . fmap (either Left (Right . f)) . runExceptT

instance Monad m => Applicative (ExceptT e m) where
  pure a = ExceptT $ return (Right a)
  ExceptT f <*> ExceptT v = ExceptT $ do
      mf <- f
      case mf of
          Left e -> return (Left e)
          Right k -> do
              mv <- v
              case mv of
                  Left e -> return (Left e)
                  Right x -> return (Right (k x))

instance (Monad m, Monoid e) => Alternative (ExceptT e m) where
  empty = ExceptT $ return (Left mempty)
  ExceptT mx <|> ExceptT my = ExceptT $ do
      ex <- mx
      case ex of
          Left e -> fmap (either (Left . mappend e) Right) my
          Right x -> return (Right x)

instance (Monad m) => Monad (ExceptT e m) where
  return a = ExceptT $ return (Right a)

  m >>= k = ExceptT $ do
      a <- runExceptT m
      case a of
          Left e -> return (Left e)
          Right x -> runExceptT (k x)

instance MonadFail m => MonadFail (ExceptT e m) where
  fail = ExceptT . fail

instance MonadTrans (ExceptT e) where
  lift m = ExceptT (fmap Right m)

instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO

mapExceptT :: (m (Either e a) -> n (Either e' b))
        -> ExceptT e m a
        -> ExceptT e' n b
mapExceptT f m = ExceptT $ f (runExceptT m)

withExceptT :: (Monad m) => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f = mapExceptT $ fmap $ either (Left . f) Right

type Except e = ExceptT e Identity

except :: (Monad m) => Either e a -> ExceptT e m a
except = ExceptT . return

runExcept :: Except e a -> Either e a
runExcept (ExceptT m) = runIdentity m

mapExcept :: (Either e a -> Either e' b)
        -> Except e a
        -> Except e' b
mapExcept f = mapExceptT (Identity . f . runIdentity)

-- | Transform any exceptions thrown by the computation using the given
-- function (a specialization of 'withExceptT').
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept = withExceptT

throwE :: (Monad m) => e -> ExceptT e m a
throwE = ExceptT . return . Left

catchE :: (Monad m) => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
m `catchE` h = ExceptT $ do
    a <- runExceptT m
    case a of
        Left  l -> runExceptT (h l)
        Right r -> return (Right r)
