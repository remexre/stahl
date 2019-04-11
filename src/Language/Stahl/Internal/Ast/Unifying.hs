module Language.Stahl.Internal.Ast.Unifying
  ( Decl(..)
  , Expr(..)
  , GlobalName(..)
  , LocalName(..)
  ) where

import Data.Functor.Const (Const)
import Data.STRef (STRef)
import Data.Void (Void)
import Language.Stahl.Ast (GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast as Ast
import Language.Stahl.Util (Location)

type Decl s = Ast.Decl (Const Void) (STRef s) (Maybe Location) (Maybe Location)
type Expr s = Ast.Expr (STRef s) (Maybe Location)
