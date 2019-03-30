module Language.Stahl.Ast.Holed
  ( Annot(..)
  , Decl(..)
  , Expr(..)
  , GlobalName(..)
  , LocalName(..)
  , declsFromValues
  ) where

import Data.ByteString.UTF8 (ByteString)
import Data.Functor.Const (Const)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import Language.Stahl.Ast.Generic (Annot(..), GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast.Generic as Generic
import Language.Stahl.Error (Error(..))
import Language.Stahl.Util (Location)
import Language.Stahl.Util.MonadNonfatal (MonadNonfatal(..), runNonfatal)
import Language.Stahl.Value (Value(..))

type Decl aE aD = Generic.Decl (Const Void) (Const ByteString) aE aD
type Expr a = Generic.Expr (Const ByteString) a

declsFromValues :: [Value] -> ([Error], Seq (Decl Location Location))
declsFromValues = foldr helper ([], Seq.empty) . map (runNonfatal . declFromValue)
  where helper (Left es') (es, ds) = (es <> es', ds)
        helper (Right d) (es, ds) = (es, ds |> d)

declFromValue :: MonadNonfatal Error m => Value -> m (Decl Location Location)
declFromValue = undefined
