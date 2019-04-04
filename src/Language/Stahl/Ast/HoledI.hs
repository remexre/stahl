-- |Declarations and expressions as they appear in the source code, with
-- holes, as well as implicits pis, lambdas, and applications.
module Language.Stahl.Ast.HoledI
  ( Decl(..)
  , Expr(..)
  , HoledIExprCustom(..)
  , addImplicitApps
  ) where

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as BS
import Data.Functor.Const (Const(..))
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import Language.Stahl.Ast.Generic (GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast.Generic as G
import qualified Language.Stahl.Ast.Holed as Holed
import Language.Stahl.Error (Error(..), astError)
import Language.Stahl.Util (Location)
import Language.Stahl.Util.MonadNonfatal (MonadNonfatal(..), mapFatalsToNonfatals)

type Decl = G.Decl HoledIExprCustom (Const Void) (Maybe Location) (Maybe Location)
type Expr = G.Expr HoledIExprCustom (Maybe Location)

data HoledIExprCustom expr
  = Hole ByteString
  | ImplicitApp expr expr
  | ImplicitLam LocalName expr
  | ImplicitPi (Maybe LocalName) expr expr (Seq GlobalName)
  deriving Show

addImplicitApps :: Holed.Decl -> m Decl
addImplicitApps = undefined
