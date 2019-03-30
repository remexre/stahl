module Language.Stahl.Ast.Unifying
  ( Annot(..)
  , Decl(..)
  , Expr(..)
  , GlobalName(..)
  , LocalName(..)
  ) where

import Data.Functor.Const (Const)
import Data.STRef (STRef)
import Data.Void (Void)
import Language.Stahl.Ast.Generic (Annot(..), GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast.Generic as Generic

type Decl s aE aD = Generic.Decl (Const Void) (STRef s) aE aD
type Expr s a = Generic.Expr (STRef s) a
