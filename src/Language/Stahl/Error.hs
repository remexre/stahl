-- |Error types and functions.
module Language.Stahl.Error
  ( Error(..)
  , ErrorKind(..)
  , ToError(..)
  , astError
  , cause
  , chain
  , chain'
  , duplicateEntryError
  , kind
  , loc
  , missingError
  , mkError
  ) where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.Monad.Writer (execWriter)
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as BS
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Language.Stahl.Internal.Value (Value, location)
import Language.Stahl.Util (Location(..))

-- |The kind of an error.
data ErrorKind
  = ConstraintFailed !String
  | CouldntParseFile !FilePath
  | DuplicateEntry !ByteString
  | InvalidAST !ByteString !Value
  | MissingValue !ByteString
  | Other !String
  | UnsolvedConstraints !(Seq ByteString)
  | VariableNotInScope !ByteString
  deriving Eq

instance Show ErrorKind where
  show (ConstraintFailed c) = "couldn't solve constraint " <> c
  show (CouldntParseFile path) = "couldn't parse " <> show path
  show (DuplicateEntry name) = "duplicate " <> BS.toString name <> " entry"
  show (InvalidAST wanted value) = "invalid " <> BS.toString wanted <> ": " <> show value
  show (MissingValue wanted) = "missing " <> BS.toString wanted
  show (Other bs) = bs
  show (UnsolvedConstraints cs) = "unsolved constraints after typechecking:" <> cs'
    where cs' = unlines (BS.toString <$> toList cs)
  show (VariableNotInScope var) = "variable not in scope: " <> BS.toString var

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

-- |Creates an 'InvalidAST' error.
astError :: ByteString -> Value -> Error
astError wanted val = mkError (InvalidAST wanted val) (val^.location)

-- |Chains an error to an 'ExceptT'.
chain :: (Monad m, ToError e) => ExceptT e m a -> (Maybe Location, ErrorKind) -> ExceptT Error m a
chain action (loc, kind) = withExceptT (\e -> mkChainedError e loc kind) action

-- |Chains an error to a 'Maybe'.
chain' :: Monad m => Maybe a -> (Maybe Location, ErrorKind) -> ExceptT Error m a
chain' (Just x) _ = pure x
chain' Nothing (loc, kind) = undefined

-- |Creates an 'DuplicateEntry' error.
duplicateEntryError :: ByteString -> Maybe Location -> Error
duplicateEntryError wanted loc = mkError (DuplicateEntry wanted) loc

-- |Creates an 'MissingValue' error.
missingError :: ByteString -> Location -> Error
missingError wanted loc = mkError (MissingValue wanted) (Just loc)

-- |Creates an error with no cause.
mkError :: ErrorKind -> Maybe Location -> Error
mkError kind loc = Error loc kind Nothing

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
