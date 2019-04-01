module Language.Stahl.Util.Value
  ( valueAsList
  , valueAsSHL
  ) where

import Data.ByteString (ByteString)
import Language.Stahl.Error (Error(..))
import Language.Stahl.Util (Location(..))
import Language.Stahl.Util.MonadNonfatal (MonadNonfatal(..))
import Language.Stahl.Value (Value(..))

valueAsList :: MonadNonfatal Error m => (Value -> Error) -> Value -> m (Location, [Value])
valueAsList onError (Cons l h t) = (l,) . (h:) . snd <$> valueAsList onError t
valueAsList onError (Nil l) = pure (l, [])
valueAsList onError val = fatal (onError val)

valueAsSHL :: MonadNonfatal Error m => (Value -> Error) -> Value -> m (Location, ByteString, [Value])
valueAsSHL onError val = valueAsList onError val >>= \case
  (loc, Symbol _ h:t) -> pure (loc, h, t)
  _ -> fatal (onError val)
