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
import Control.Monad.Writer (MonadWriter(..), execWriter)
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as BS
import Language.Stahl.Util (Location(..))

-- |The kind of an error.
data ErrorKind
  = CouldntParseFile !FilePath
  | Other !ByteString

instance Show ErrorKind where
  show (CouldntParseFile path) = "couldn't parse " <> show path
  show (Other bs) = BS.toString bs

-- |An error, with an associated 'Location' and (optionally) a cause.
data Error = Error
  { _loc :: Maybe Location
  , _kind :: ErrorKind
  , _cause :: Maybe (Either String Error)
  }

makeLenses ''Error

instance Show Error where
  show err = execWriter $ loop err
    where loop err = do
            case err^.loc of
              Just loc -> do
                tell "At "
                tell (show loc)
                tell ", "
              Nothing -> pure ()
            tell $ show $ err^.kind
            case err^.cause of
              Just c -> do
                tell "\nCause: "
                case c of
                  Left msg -> tell msg
                  Right err -> loop err
              Nothing -> pure ()

-- |Values that can be converted into the 'Error' type.
class ToError a where
  mkChainedError :: a -> Maybe Location -> ErrorKind -> Error
  default mkChainedError :: Show a => a -> Maybe Location -> ErrorKind -> Error
  mkChainedError err loc kind = Error loc kind (Just (Left (show err)))

instance ToError Error where
  mkChainedError err loc kind = Error loc kind (Just (Right err))

instance ToError [Char] where
  mkChainedError err loc kind = Error loc kind (Just (Left err))

instance ToError ByteString
