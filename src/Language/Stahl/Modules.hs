module Language.Stahl.Modules
  ( LibMeta(..)
  , LibName(..)
  , Library(..)
  , Module(..)
  , decls
  , deps
  , exports
  , imports
  , libMeta
  , libName
  , library
  , loadLibMeta
  , loadLibrary
  , major
  , minor
  , modName
  , mods
  , name
  , patch
  , path
  ) where

import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString, readFile)
import Data.Either (either)
import Data.Functor.Const (Const)
import Data.Void (Void)
import Language.Stahl.Error (Error(..))
import Language.Stahl.Modules.FromValue (libMetaFromValues)
import Language.Stahl.Modules.Types
  ( LibMeta(..)
  , LibName(..)
  , Library(..)
  , Module(..)
  , decls
  , deps
  , exports
  , imports
  , libMeta
  , libName
  , library
  , major
  , minor
  , modName
  , mods
  , name
  , patch
  , path
  )
import qualified Language.Stahl.Parser
import Language.Stahl.Util (Location, wholeFile)
import Language.Stahl.Util.MonadNonfatal (MonadNonfatal(..))
import Language.Stahl.Value (Value)
import Prelude hiding (readFile)
import System.FilePath ((</>))

loadLibrary :: (MonadIO m, MonadNonfatal Error m) => FilePath -> m (Library (Const Void) (Const Void) (Maybe Location) (Maybe Location))
loadLibrary path = do
  meta <- loadLibMeta (path </> "lib.stahld")
  undefined

loadLibMeta :: (MonadIO m, MonadNonfatal Error m) => FilePath -> m LibMeta
loadLibMeta path = do
  src <- liftIO $ readFile path
  libMetaFromValues (wholeFile path src) =<< parse path src

parse :: (MonadIO m, MonadNonfatal Error m) => FilePath -> ByteString -> m [Value]
parse path = either fatal pure <=< liftIO . runExceptT . Language.Stahl.Parser.parse path
