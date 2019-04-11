{-# LANGUAGE Rank2Types #-}

module Language.Stahl.Internal.Modules.FromValue
  ( libMetaFromValues
  , moduleHeaderFromValues
  ) where

import Control.Lens (Lens', (.=), (^.), (^?), _1, _2, _3, _Just, use)
import Control.Lens.Extras (is)
import Control.Lens.TH (makeLenses)
import Control.Monad ((<=<), mapM, mapM_)
import Control.Monad.State (execStateT)
import Control.Monad.State.Class (MonadState(..))
import Data.ByteString.UTF8 (ByteString)
import Data.Default (Default(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word)
import Language.Stahl.Error (Error, astError, duplicateEntryError, missingError)
import Language.Stahl.Internal.Modules.Types (LibMeta(..), LibName(..))
import Language.Stahl.Internal.Util.MonadNonfatal (MonadNonfatal(..), mapFatalsToNonfatals)
import Language.Stahl.Internal.Util.Value (valueAsList, valueAsSHL, valueAsSymList)
import Language.Stahl.Internal.Value (Value(..))
import Language.Stahl.Util (Location)

data LibMetaItem
  = Deps !(Maybe Location) !(Map ByteString LibName)
  | Description !(Maybe Location) !ByteString
  | Name !(Maybe Location) !ByteString
  | Version !(Maybe Location) !(Word, Word, Word)
  deriving Show

data LibMetaPartial = LibMetaPartial
  { _deps :: Maybe (Map ByteString LibName)
  , _description :: Maybe ByteString
  , _name :: Maybe ByteString
  , _version :: Maybe (Word, Word, Word)
  }

makeLenses ''LibMetaPartial

instance Default LibMetaPartial where
  def = LibMetaPartial Nothing Nothing Nothing Nothing

freeze :: MonadNonfatal Error m => Location -> LibMetaPartial -> m LibMeta
freeze loc partial =
    LibMeta <$> (LibName <$> orMissing "name" (partial^.name)
                         <*> orMissing "version" (partial^?version._Just._1)
                         <*> orMissing "version" (partial^?version._Just._2)
                         <*> orMissing "version" (partial^?version._Just._3))
            <*> orMissing "deps" (partial^.deps)
  where orMissing _ (Just x) = pure x
        orMissing missing Nothing = fatal (missingError missing loc)

addItem :: (MonadNonfatal Error m, MonadState LibMetaPartial m) => LibMetaItem -> m ()
addItem (Deps loc ds) = setIfMaybe "deps" deps loc ds
addItem (Description loc d) = setIfMaybe "description" description loc d
addItem (Name loc n) = setIfMaybe "name" name loc n
addItem (Version loc v) = setIfMaybe "version" version loc v

setIfMaybe :: (MonadNonfatal Error m, MonadState LibMetaPartial m) => ByteString -> Lens' LibMetaPartial (Maybe a) -> Maybe Location -> a -> m ()
setIfMaybe fieldName lens loc val = do
  alreadyPresent <- is _Just <$> use lens
  if alreadyPresent then
    fatal (duplicateEntryError fieldName loc)
  else
    lens .= Just val

moduleHeaderFromValues :: MonadNonfatal Error m => Location -> [Value]
                       -> m (ByteString, Set ByteString, [(ByteString, Set ByteString)], [Value])
moduleHeaderFromValues loc [] = fatal (missingError "module form" loc)
moduleHeaderFromValues loc (h:t) = do
  (name, exports) <- moduleFormFromValue h
  (imports, rest) <- importFormsFromValues t
  pure (name, exports, imports, rest)

moduleFormFromValue :: MonadNonfatal Error m => Value -> m (ByteString, Set ByteString)
moduleFormFromValue val = valueAsSymList (astError "module form") val >>= \case
  (_, ("module":name:exports)) -> pure (name, Set.fromList exports)
  _ -> fatal (astError "module form" val)

importFormsFromValues :: MonadNonfatal Error m => [Value] -> m ([(ByteString, Set ByteString)], [Value])
importFormsFromValues vals = do
  let isImportForm (Cons _ (Symbol _ "import") _) = True
      isImportForm _ = False
  let (imports, rest) = span isImportForm vals
  imports' <- mapM importFormFromValue imports
  pure (imports', rest)

importFormFromValue :: MonadNonfatal Error m => Value -> m (ByteString, Set ByteString)
importFormFromValue val = valueAsSHL (astError "import form") val >>= \case
  (_, "import", [Symbol _ name, imports]) -> fmap ((name,) . Set.fromList . snd) $
                                             valueAsSymList (astError "import list") imports
  _ -> fatal (astError "import form" val)

libMetaFromValues :: MonadNonfatal Error m => Location -> [Value] -> m LibMeta
libMetaFromValues loc = freeze loc
                    <=< flip execStateT def . mapM_ addItem
                    <=< mapFatalsToNonfatals libMetaItemFromValue

libMetaItemFromValue :: MonadNonfatal Error m => Value -> m LibMetaItem
libMetaItemFromValue val = valueAsSHL (astError "library metadata") val >>= \case
  (loc, "deps", deps) -> Deps loc <$> depsFromValues deps
  (loc, "description", [String _ desc]) -> pure $ Description loc desc
  (loc, "name", [Symbol _ name]) -> pure $ Name loc name
  (loc, "version", [version]) -> Version loc <$> versionFromValue version
  _ -> fatal (astError "library metadata" val)

depsFromValues :: MonadNonfatal Error m => [Value] -> m (Map ByteString LibName)
depsFromValues = fmap (Map.fromList . map (\ln -> (Language.Stahl.Internal.Modules.Types._name ln, ln))) . mapFatalsToNonfatals depsFromValue

depsFromValue :: MonadNonfatal Error m => Value -> m LibName
depsFromValue val = valueAsList (astError "deps") val >>= \case
  (_, [Symbol _ name, version]) -> (\(ma, mi, pa) -> LibName name ma mi pa) <$> versionFromValue version
  _ -> fatal (astError "deps" val)

versionFromValue :: MonadNonfatal Error m => Value -> m (Word, Word, Word)
versionFromValue val = valueAsList (astError "version") val >>= \case
  (_, [Int _ major, Int _ minor, Int _ patch]) -> pure (fromIntegral major, fromIntegral minor, fromIntegral patch)
  _ -> fatal (astError "version" val)
