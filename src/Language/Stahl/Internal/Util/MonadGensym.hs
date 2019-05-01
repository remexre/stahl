{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances #-}

module Language.Stahl.Internal.Util.MonadGensym
  ( MonadGensym(..)
  , GensymT(..)
  , runGensym
  , runGensymT
  ) where

import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State (StateT(..), evalStateT)
import Control.Monad.State.Class (MonadState(..), modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Writer (WriterT(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as BS
import Data.Functor.Identity (Identity(..))

newtype GensymT m a = GensymT
  { unGensymT :: StateT Int m a
  } deriving (Applicative, Functor, Monad, MonadFix)

deriving instance MonadReader r m => MonadReader r (GensymT m)
deriving instance MonadWriter w m => MonadWriter w (GensymT m)

instance MonadTrans GensymT where
  lift = GensymT . lift

runGensym :: GensymT Identity a -> a
runGensym = runIdentity . runGensymT

runGensymT :: Monad m => GensymT m a -> m a
runGensymT = flip evalStateT 0 . unGensymT

class Monad m => MonadGensym m where
  genInt :: m Int

  genSym :: m ByteString
  genSym = ("_" <>) . BS.fromString . show <$> genInt

  default genInt :: (MonadGensym m', MonadTrans t, m ~ t m') => m Int
  genInt = lift genInt

instance Monad m => MonadGensym (GensymT m) where
  genInt = GensymT $ do
    x <- get
    modify (1+)
    pure x

instance (MonadGensym m, Monoid w) => MonadGensym (WriterT w m)
