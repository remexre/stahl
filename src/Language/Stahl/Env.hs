{-# LANGUAGE Rank2Types, StandaloneDeriving, UndecidableInstances #-}

module Language.Stahl.Env
  ( Env(..)
  , extendEnvWith
  , extendEnvWith'
  , lookupTy
  , lookupVal
  ) where

import Control.Lens (Getter, (^.), over)
import Control.Lens.TH (makeLenses)
import Control.Monad.Reader.Class (MonadReader(..))
import Data.ByteString.UTF8 (ByteString)
import Data.Default (Default(..))
import Data.Map (Map)
import Language.Stahl.Ast (Expr(..), LocalName(..))
import Language.Stahl.Error (Error, ErrorKind(..), mkError)
import Language.Stahl.Modules.Types (LibName(..))
import Language.Stahl.Util (Location(..))
import Language.Stahl.Util.MonadNonfatal (MonadNonfatal(..))

-- |An entry in the environment.
data EnvEntry c a = EnvEntry 
  { _name :: ByteString
  , _ty :: Expr c a
  , _val :: Maybe (Expr c a)
  }

makeLenses ''EnvEntry

deriving instance (Show a, Show (c (Expr c a))) => Show (EnvEntry c a)

-- |The type of an environment.
data Env c a = Env
  { _globals :: Map LibName (Map ByteString (Map ByteString (EnvEntry c a)))
  , _locals :: [EnvEntry c a]
  }

makeLenses ''Env

instance Default (Env c a) where def = Env def def
deriving instance (Show a, Show (c (Expr c a))) => Show (Env c a)

extendEnvWith :: LocalName -> Expr c a -> Maybe (Expr c a) -> Env c a -> Env c a
extendEnvWith (LocalName n) t e = over locals (EnvEntry n t e:)

extendEnvWith' :: Maybe LocalName -> Expr c a -> Maybe (Expr c a) -> Env c a -> Env c a
extendEnvWith' (Just (LocalName n)) t e = over locals (EnvEntry n t e:)
extendEnvWith' Nothing _ _ = id

class EnvName n where
  lookupTy :: (MonadNonfatal Error m, MonadReader (Env c a) m) => n -> Maybe Location -> m (Expr c a)
  lookupVal :: (MonadNonfatal Error m, MonadReader (Env c a) m) => n -> Maybe Location -> m (Maybe (Expr c a))

envLookup' :: MonadNonfatal Error m => Getter (EnvEntry c a) b
           -> ByteString -> Maybe Location -> Env c a -> m b
envLookup' lens n loc env =
  case filter (\entry -> n == entry^.name) (env^.locals) of
    h:_ -> pure (h^.lens)
    [] -> fatal (mkError (VariableNotInScope n) loc)

instance EnvName LocalName where
  lookupTy (LocalName n) loc = envLookup' ty n loc =<< ask
  lookupVal (LocalName n) loc = envLookup' val n loc =<< ask
