module Language.Stahl.Ast.Holed
  ( Annot(..)
  , Decl(..)
  , Expr(..)
  , GlobalName(..)
  , LocalName(..)
  ) where

import Data.ByteString.UTF8 (ByteString)
import Data.Functor.Const (Const)
import Data.Void (Void)
import Language.Stahl.Ast.Generic (Annot(..), GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast.Generic as Generic

type Decl s aE aD = Generic.Decl (Const Void) (Const ByteString) aE aD
type Expr s a = Generic.Expr (Const ByteString) a
