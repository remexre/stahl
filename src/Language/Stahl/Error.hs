{-# LANGUAGE DefaultSignatures, FlexibleContexts, TemplateHaskell #-}

module Language.Stahl.Error
  ( Error(..)
  , ErrorKind(..)
  , Location(..)
  , ToError(..)
  , chain
  ) where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, withExceptT)
import Control.Monad.Writer (MonadWriter(..), execWriter)
import Data.ByteString.UTF8 (ByteString)
import Data.Maybe (isJust)

data ErrorKind
  = CouldntParseFile ByteString
  | Other ByteString
  deriving Show

data Location
  = Point
    { _file :: !ByteString
    , _line :: !Word
    , _col :: !Word
    }
  | Span
    { _file :: !ByteString
    , _lineStart :: !Word
    , _colStart :: !Word
    , _lineEnd :: !Word
    , _colEnd :: !Word
    }
  deriving (Eq, Show)

makeLenses ''Location

data Error = Error
  { _loc :: Maybe Location
  , _kind :: ErrorKind
  , _cause :: Maybe (Either String Error)
  }

makeLenses ''Error

instance Show Error where
  show error = execWriter $ do
      when (isJust $ error^.cause) (tell "      ")
      loop error
    where loop error = do
            case error^.loc of
              Just loc -> do
                tell "At "
                tell (show loc)
              Nothing -> pure ()
            tell $ show $ error^.kind
            case error^.cause of
              Just (Left msg) -> tell "Cause: "
              Just (Right error) -> loop error
              Nothing -> pure ()

class ToError a where
  mkChainedError :: a -> Maybe Location -> ErrorKind -> Error
  default mkChainedError :: Show a => a -> Maybe Location -> ErrorKind -> Error
  mkChainedError err loc kind = Error loc kind (Just (Left (show err)))

instance ToError Error where
  mkChainedError err loc kind = Error loc kind (Just (Right err))

-- |Chains an error based on @ErrorKind@ to the existing error.
chain :: (ToError e, Functor m) => ExceptT e m a -> (Maybe Location, ErrorKind) -> ExceptT Error m a
chain x (loc, kind) = withExceptT (\err -> mkChainedError err loc kind) x
