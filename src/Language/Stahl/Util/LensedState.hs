{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, Rank2Types,
             StandaloneDeriving, UndecidableInstances #-}

module Language.Stahl.Util.LensedState
  ( LensedStateT(..)
  , liftLensedStateT
  , runLensedStateT
  ) where

import Control.Lens (Lens', ReifiedLens(..), ReifiedLens', assign, use)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.State.Strict (StateT(..))

newtype LensedStateT s s' m a = LensedStateT
  { unLensedStateT :: ReaderT (ReifiedLens' s s') (StateT s m) a
  } deriving (Functor, Applicative, Monad)

deriving instance MonadError e m => MonadError e (LensedStateT s s' m)

liftLensedStateT :: Lens' s s' -> LensedStateT s s' m a -> StateT s m a
liftLensedStateT lens (LensedStateT m) = runReaderT m (Lens lens)

runLensedStateT :: Lens' s s' -> LensedStateT s s' m a -> s -> m (a, s)
runLensedStateT lens = runStateT . liftLensedStateT lens

instance Monad m => MonadState s' (LensedStateT s s' m) where
  get = LensedStateT (use . runLens =<< ask)
  put x = LensedStateT (flip assign x . runLens =<< ask)
