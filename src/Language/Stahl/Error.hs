-- |Error types and functions.
module Language.Stahl.Error
  ( Error(..)
  , ErrorKind(..)
  , ToError(..)
  , cause
  , kind
  , loc
  ) where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (when)
import Control.Monad.Writer (MonadWriter(..), execWriter)
import Data.ByteString.UTF8 (ByteString)
import Data.Maybe (isJust)
import Language.Stahl.Util (Location(..))

-- |The kind of an error.
data ErrorKind
  = CouldntParseFile !FilePath
  | Other !ByteString
  deriving Show

-- |An error, with an associated 'Location' and (optionally) a cause.
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

-- |Values that can be converted into the 'Error' type.
class ToError a where
  mkChainedError :: a -> Maybe Location -> ErrorKind -> Error
  default mkChainedError :: Show a => a -> Maybe Location -> ErrorKind -> Error
  mkChainedError err loc kind = Error loc kind (Just (Left (show err)))

instance ToError Error where
  mkChainedError err loc kind = Error loc kind (Just (Right err))
