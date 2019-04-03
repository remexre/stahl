module Language.Stahl.Ast.Unholed
  ( Decl(..)
  , Expr(..)
  ) where

import Data.Functor.Const (Const)
import Data.Sequence (Seq)
import Data.Void (Void)
import Language.Stahl.Ast.Generic (GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast.Generic as G
import Language.Stahl.Util (Location)

type Decl = G.Decl UnholedExprCustom (Const Void) (Maybe Location) (Maybe Location)
type Expr = G.Expr UnholedExprCustom (Maybe Location)

data UnholedExprCustom expr
  = ImplicitLam LocalName expr
  | ImplicitPi (Maybe LocalName) expr expr (Seq GlobalName)
