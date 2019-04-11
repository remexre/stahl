module Language.Stahl.Internal.Util.Value
  ( PP(..)
  , valueAsList
  , valueAsList'
  , valueAsSHL
  , valueAsSymList
  , valueAsSymList'
  ) where

import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Sequence (Seq)
import Data.Void (Void, absurd)
import Language.Stahl.Error (Error)
import Language.Stahl.Internal.Util.MonadNonfatal (MonadNonfatal(..), runNonfatal)
import Language.Stahl.Internal.Value (Value(..))
import Language.Stahl.Util (Location(..), eitherToMaybe)

-- |A class for pretty-printing as 'Value's.
class PP a where
  pp :: a -> Value
  showPP :: a -> String
  showPP = show . pp

instance PP [Value] where
  pp [] = Nil Nothing
  pp (h:t) = Cons Nothing h (pp t)

instance PP a => PP (Seq a) where
  pp = pp . map pp . toList

instance PP (f (g a)) => PP (Compose f g a) where
  pp = pp . getCompose

instance PP a => PP (Const a b) where
  pp = pp . getConst

instance (PP a, PP b) => PP (Either a b) where
  pp (Left x) = pp x
  pp (Right x) = pp x

instance PP Void where
  pp = absurd

valueAsList :: MonadNonfatal Error m => (Value -> Error) -> Value -> m (Maybe Location, [Value])
valueAsList onError (Cons l h t) = (l,) . (h:) . snd <$> valueAsList onError t
valueAsList onError (Nil l) = pure (l, [])
valueAsList onError val = fatal (onError val)

valueAsSHL :: MonadNonfatal Error m => (Value -> Error) -> Value -> m (Maybe Location, ByteString, [Value])
valueAsSHL onError val = valueAsList onError val >>= \case
  (loc, Symbol _ h:t) -> pure (loc, h, t)
  _ -> fatal (onError val)

valueAsSymList :: MonadNonfatal Error m => (Value -> Error) -> Value -> m (Maybe Location, [ByteString])
valueAsSymList onError (Cons l (Symbol _ h) t) = (l,) . (h:) . snd <$> valueAsSymList onError t
valueAsSymList onError (Nil l) = pure (l, [])
valueAsSymList onError val = fatal (onError val)

valueAsList' :: Value -> Maybe [Value]
valueAsList' = fmap snd . eitherToMaybe . runNonfatal . valueAsList undefined

valueAsSymList' :: Value -> Maybe [ByteString]
valueAsSymList' = fmap snd . eitherToMaybe . runNonfatal . valueAsSymList undefined
