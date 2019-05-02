{-# LANGUAGE FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, StandaloneDeriving,
             UndecidableInstances #-}

module Language.Stahl.Internal.Util.MonadNonfatal
  ( MonadNonfatal(..)
  , NonfatalT(..)
  , mapFatalsToNonfatals
  , runNonfatal
  , runNonfatal'
  , runNonfatalT
  , runNonfatalT'
  ) where

import Control.Monad (forM_)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State (StateT(..))
import Control.Monad.State.Class (gets, modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Writer (WriterT(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Functor.Identity (Identity(..))
import Language.Stahl.Internal.Util.MonadGensym (GensymT(..), MonadGensym)

newtype NonfatalT e m a = NonfatalT
  { unNonfatalT :: ExceptT () (StateT [e] m) a
  } deriving (Applicative, Functor, Monad, MonadFix)

deriving instance MonadIO m => MonadIO (NonfatalT e m)
deriving instance MonadReader r m => MonadReader r (NonfatalT e m)
deriving instance MonadWriter w m => MonadWriter w (NonfatalT e m)

instance MonadTrans (NonfatalT e) where
  lift = NonfatalT . lift . lift

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
  catch :: m a -> m ([e], Maybe a)
  errorsExist :: m Bool
  raise :: e -> m ()

  fatal :: e -> m a
  fatal err = raise err >> abort
  fatallyFromEither :: Either [e] a -> m a
  fatallyFromEither (Left errs) = raiseAll errs >> abort
  fatallyFromEither (Right x) = pure x
  raiseAll :: [e] -> m ()
  raiseAll = mapM_ raise

  default abort :: (MonadNonfatal e m', MonadTrans t, m ~ t m') => m a
  abort = lift abort
  default errorsExist :: (MonadNonfatal e m', MonadTrans t, m ~ t m') => m Bool
  errorsExist = lift errorsExist
  default raise :: (MonadNonfatal e m', MonadTrans t, m ~ t m') => e -> m ()
  raise = lift . raise

instance Monad m => MonadNonfatal e (NonfatalT e m) where
  abort = NonfatalT $ throwError ()
  catch = lift . runNonfatalT'
  errorsExist = NonfatalT $ gets (not . null)
  raise err = NonfatalT $ modify (err:)

instance MonadNonfatal e m => MonadNonfatal e (GensymT m) where
  catch = GensymT . catch . unGensymT

instance MonadNonfatal e m => MonadNonfatal e (ReaderT s m) where
  catch = ReaderT . (\act r -> catch (act r)) . runReaderT

instance MonadNonfatal e m => MonadNonfatal e (StateT s m) where
  catch = StateT . (\act s -> (,s) <$> catch (fst <$> act s)) . runStateT

instance (MonadNonfatal e m, Monoid w) => MonadNonfatal e (WriterT w m) where
  catch = WriterT . fmap helper . catch . runWriterT
    where helper (es, Just (x, w)) = ((es, Just x), w)
          helper (es, Nothing) = ((es, Nothing), mempty)

instance MonadGensym m => MonadGensym (NonfatalT e m)

mapFatalsToNonfatals :: MonadNonfatal e m => (a -> m b) -> [a] -> m [b]
mapFatalsToNonfatals f (h:t) = do
    (es, h') <- catch (f h)
    forM_ es raise
    consMaybe h' <$> mapFatalsToNonfatals f t
  where consMaybe (Just h') t' = h':t'
        consMaybe Nothing t' = t'
mapFatalsToNonfatals _ [] = pure []
