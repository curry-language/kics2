module Control.Monad.Trans.State where

import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype StateT s m a = StateT {
    runStateT :: s -> m (a, s)
  }

instance Monad m => Functor (StateT s m) where
  fmap f m = StateT $ \ s -> do (v, s') <- runStateT m s
                                return (f v, s')

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \ s -> return (x, s)

instance Monad m => Monad (StateT s m) where
  return = pure
  m >>= fm = StateT $ \ s -> do (v ,s') <- runStateT m s
                                runStateT (fm v) s'

instance MonadFail m => MonadFail (StateT s m) where
  fail msg = StateT $ \ _ -> fail msg

instance  MonadTrans (StateT s) where
  lift m = StateT $ \ s -> do v <- m
                              return (v, s)

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO

get :: Monad m => StateT s m s
get = StateT $ \s -> return (s, s)

put :: Monad m => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT $ \s -> return (f s)

modify :: Monad m => (s -> s) -> StateT  s m ()
modify f = StateT $ \s -> return ((), f s)

gets :: Monad m => (s -> a) -> StateT s m a
gets f = StateT $ \ s -> return (f s, s)

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m = fmap fst . runStateT m

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m = fmap snd . runStateT m

mapStateT :: (Monad m, Monad n)
          => (m (a, s) -> n (b, s))
          -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ \s -> f (runStateT m s)

withStateT :: Monad m => (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT $ \ s -> do (v, s') <- runStateT m s
                                    return (v, f s')

type State s a = StateT s Identity a

runState :: State s a -> s -> (a, s)
runState m = runIdentity . runStateT m

evalState :: State s a -> s -> a
evalState m = runIdentity . evalStateT m

execState :: State s a -> s -> s
execState m = runIdentity . execStateT m

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f = mapStateT (return . f . runIdentity)

withState :: (s -> s) -> State s a -> State s a
withState = withStateT
