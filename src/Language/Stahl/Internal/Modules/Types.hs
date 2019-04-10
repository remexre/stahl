{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}

module Language.Stahl.Internal.Modules.Types
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
  ) where

import Control.Lens.TH (makeLenses)
import Data.ByteString.UTF8 (ByteString)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Language.Stahl.Ast (Decl, Expr)

-- |The name and version of a library.
data LibName = LibName
  { _name :: ByteString
  , _major :: Word
  , _minor :: Word
  , _patch :: Word
  } deriving (Eq, Ord, Show)

makeLenses ''LibName

-- |A module.
data Module cE cD aE aD = Module
  { _library :: LibName
  , _modName :: ByteString
  , _exports :: Set ByteString
  , _imports :: Map LibName (Map ByteString (Set ByteString))
  , _decls :: Seq (Decl cE cD aE aD)
  }

deriving instance (Show aD, Show aE, Show (cD (Decl cE cD aE aD)), Show (cE (Expr cE aE))) => Show (Module cE cD aE aD)

makeLenses ''Module

-- |Metadata about a library, loaded from lib.stahld.
data LibMeta = LibMeta
  { _libName :: LibName
  , _deps :: Map ByteString LibName
  } deriving (Eq, Ord, Show)

makeLenses ''LibMeta

-- |A library.
data Library cE cD aE aD = Library
  { _libMeta :: LibMeta
  , _mods :: Map ByteString (Module cE cD aE aD)
  , _path :: Maybe FilePath
  }

deriving instance (Show aD, Show aE, Show (cD (Decl cE cD aE aD)), Show (cE (Expr cE aE))) => Show (Library cE cD aE aD)

makeLenses ''Library
