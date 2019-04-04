module Language.Stahl.Ast.Unifying
  ( Decl(..)
  , Expr(..)
  , GlobalName(..)
  , LocalName(..)
  ) where

import Data.Functor.Const (Const)
import Data.STRef (STRef)
import Data.Void (Void)
import Language.Stahl.Ast.Generic (GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast.Generic as Generic
import Language.Stahl.Util (Location)

type Decl s = Generic.Decl (Const Void) (STRef s) (Maybe Location) (Maybe Location)
type Expr s = Generic.Expr (STRef s) (Maybe Location)
