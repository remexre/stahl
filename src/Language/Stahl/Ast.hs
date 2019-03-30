module Language.Stahl.Ast
  ( Annot(..)
  , Decl(..)
  , Expr(..)
  , GlobalName(..)
  , LocalName(..)
  ) where

import Data.Functor.Const (Const)
import Data.Void (Void)
import Language.Stahl.Ast.Generic (Annot(..), GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast.Generic as Generic

type Decl s a = Generic.Decl (Const Void) (Const Void) a
type Expr s a = Generic.Expr (Const Void) a
