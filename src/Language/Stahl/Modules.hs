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

import Control.Lens ((^.))
import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString, readFile)
import qualified Data.ByteString as BS (intercalate)
import qualified Data.ByteString.UTF8 as BS
import Data.Default (Default(..))
import Data.Either (either)
import Data.Foldable (toList)
import Data.Functor.Const (Const)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import Language.Stahl.Error (Error)
import Language.Stahl.Internal.Ast.Holed (declsFromValues)
import Language.Stahl.Internal.Ast.HoledI (addImplicitApps)
import Language.Stahl.Internal.Ast.Unholed (ExprCustom, solveDeclForHoles)
import Language.Stahl.Internal.Modules.FromValue (libMetaFromValues, moduleHeaderFromValues)
import Language.Stahl.Internal.Modules.Types
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
import qualified Language.Stahl.Internal.Parser
import Language.Stahl.Internal.Util (Location, wholeFile)
import Language.Stahl.Internal.Util.MonadNonfatal (MonadNonfatal(..))
import Language.Stahl.Internal.Value (Value, isSymbolish)
import Prelude hiding (readFile)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), isExtensionOf)

loadLibrary :: (MonadIO m, MonadNonfatal Error m) => FilePath
            -> m (Library ExprCustom (Const Void) (Maybe Location) (Maybe Location))
loadLibrary path = do
  meta <-  loadLibMeta (path </> "lib.stahld")
  Library meta <$> loadModules (meta^.libName) Seq.empty path <*> (pure $ Just path)

loadLibMeta :: (MonadIO m, MonadNonfatal Error m) => FilePath -> m LibMeta
loadLibMeta path = do
  src <- liftIO $ readFile path
  libMetaFromValues (wholeFile path src) =<< parse path src

loadModules :: (MonadIO m, MonadNonfatal Error m) => LibName -> Seq ByteString -> FilePath
            -> m (Map ByteString (Module ExprCustom (Const Void) (Maybe Location) (Maybe Location)))
loadModules libName modParts path = fmap mconcat . mapM helper =<< (liftIO $ listDirectory path)
  where helper subpath = do
          let path' = path </> subpath
          let modParts' = modParts |> BS.fromString subpath
          isFile <- liftIO $ doesFileExist path'
          isDir <- liftIO $ doesDirectoryExist path'
          if not (all isSymbolish subpath) || ':' `elem` subpath then
            pure Map.empty
          else if isFile && ".stahl" `isExtensionOf` path' then
            Map.singleton (BS.intercalate ":" $ toList modParts') <$> loadModule libName modParts' path'
          else if isDir then
            loadModules libName modParts' path'
          else
            pure Map.empty

loadModule :: (MonadIO m, MonadNonfatal Error m) => LibName -> Seq ByteString -> FilePath
           -> m (Module ExprCustom (Const Void) (Maybe Location) (Maybe Location))
loadModule libName expectedName path = do
  src <- liftIO $ readFile path
  (name, exports, imports, body) <- moduleHeaderFromValues (wholeFile path src) =<< parse path src
  decls <- declsFromValues body
  decls' <- mapM (solveDeclForHoles <=< flip runReaderT def . addImplicitApps) decls

  let splitOffLibPart sym = do
        let (modPart, name) = BS.break (== ':') sym
        error $ show (modPart, name)
        undefined

  -- imports' <- fmap (_ . map (uncurry Map.singleton)) $ _ imports
  pure $ Module libName name exports (undefined imports) decls'

parse :: (MonadIO m, MonadNonfatal Error m) => FilePath -> ByteString -> m [Value]
parse path = either fatal pure <=< liftIO . runExceptT . Language.Stahl.Internal.Parser.parse path
