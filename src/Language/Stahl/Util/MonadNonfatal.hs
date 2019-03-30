{-# LANGUAGE FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             StandaloneDeriving, UndecidableInstances #-}

module Language.Stahl.Util.MonadNonfatal
  ( MonadNonfatal(..)
  , NonfatalT(..)
  , runNonfatal
  , runNonfatal'
  , runNonfatalT
  , runNonfatalT'
  ) where

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (StateT(..))
import Control.Monad.State.Class (gets, modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Functor.Identity (Identity(..))

newtype NonfatalT e m a = NonfatalT
  { unNonfatalT :: ExceptT () (StateT [e] m) a
  } deriving (Applicative, Functor, Monad, MonadFix)

deriving instance MonadIO m => MonadIO (NonfatalT e m)
deriving instance MonadWriter w m => MonadWriter w (NonfatalT e m)

runNonfatal :: NonfatalT e Identity a -> Either [e] a
runNonfatal = runIdentity . runNonfatalT

runNonfatal' :: NonfatalT e Identity a -> ([e], Maybe a)
runNonfatal' = runIdentity . runNonfatalT'

runNonfatalT :: Monad m => NonfatalT e m a -> m (Either [e] a)
runNonfatalT = fmap helper . flip runStateT [] . runExceptT . unNonfatalT
  where helper (Right x, []) = Right x
        helper (_, errs) = Left errs

runNonfatalT' :: Monad m => NonfatalT e m a -> m ([e], Maybe a)
runNonfatalT' = fmap helper . flip runStateT [] . runExceptT . unNonfatalT
  where helper (Left (), es) = (es, Nothing)
        helper (Right x, es) = (es, Just x)

class Monad m => MonadNonfatal e m | m -> e where
  abort :: m a
  errorsExist :: m Bool
  raise :: e -> m ()

  fatal :: e -> m a
  fatal err = raise err >> abort

  default abort :: (MonadNonfatal e m', MonadTrans t, m ~ t m') => m a
  abort = lift abort
  default errorsExist :: (MonadNonfatal e m', MonadTrans t, m ~ t m') => m Bool
  errorsExist = lift errorsExist
  default raise :: (MonadNonfatal e m', MonadTrans t, m ~ t m') => e -> m ()
  raise = lift . raise

instance Monad m => MonadNonfatal e (NonfatalT e m) where
  abort = NonfatalT $ throwError ()
  errorsExist = NonfatalT $ gets (not . null)
  raise err = NonfatalT $ modify (err:)

instance MonadNonfatal e m => MonadNonfatal e (StateT s m)
