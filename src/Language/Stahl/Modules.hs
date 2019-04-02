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
import qualified Data.ByteString as BS (intercalate)
import qualified Data.ByteString.UTF8 as BS
import Data.Either (either)
import Data.Foldable (toList)
import Data.Functor.Const (Const)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
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
import Language.Stahl.Value (Value, isSymbolish)
import Prelude hiding (readFile)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), isExtensionOf)

import Debug.Trace

loadLibrary :: (MonadIO m, MonadNonfatal Error m) => FilePath -> m (Library (Const Void) (Const Void) (Maybe Location) (Maybe Location))
loadLibrary path = Library <$> loadLibMeta (path </> "lib.stahld") <*> loadModules Seq.empty path <*> (pure $ Just path)

loadLibMeta :: (MonadIO m, MonadNonfatal Error m) => FilePath -> m LibMeta
loadLibMeta path = do
  src <- liftIO $ readFile path
  libMetaFromValues (wholeFile path src) =<< parse path src

loadModules :: (MonadIO m, MonadNonfatal Error m) => Seq ByteString -> FilePath -> m (Map ByteString (Module (Const Void) (Const Void) (Maybe Location) (Maybe Location)))
loadModules modParts path = fmap mconcat . mapM helper =<< (liftIO $ listDirectory path)
  where helper subpath = do
          let path' = path </> subpath
          let modParts' = modParts |> BS.fromString subpath
          isFile <- liftIO $ doesFileExist path'
          isDir <- liftIO $ doesDirectoryExist path'
          if not (all isSymbolish subpath) || ':' `elem` subpath then
            pure Map.empty
          else if isFile && ".stahl" `isExtensionOf` path' then
            Map.singleton (BS.intercalate ":" $ toList modParts') <$> loadModule modParts' path'
          else if isDir then
            loadModules modParts' path'
          else
            pure Map.empty

loadModule :: (MonadIO m, MonadNonfatal Error m) => Seq ByteString -> FilePath -> m (Module (Const Void) (Const Void) (Maybe Location) (Maybe Location))
loadModule modPath path = do
  src <- parse path =<< (liftIO $ readFile path)
  error ("loadModule " <> path <> "\n" <> unlines (map show src))

parse :: (MonadIO m, MonadNonfatal Error m) => FilePath -> ByteString -> m [Value]
parse path = either fatal pure <=< liftIO . runExceptT . Language.Stahl.Parser.parse path
