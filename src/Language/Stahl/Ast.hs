module Language.Stahl.Ast
  ( Decl(..)
  , Expr(..)
  , GlobalName(..)
  , LocalName(..)
  ) where

import Data.Functor.Const (Const)
import Data.Void (Void)
import Language.Stahl.Ast.Generic (GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast.Generic as Generic

type Decl = Generic.Decl (Const Void) (Const Void) () ()
type Expr = Generic.Expr (Const Void) ()
