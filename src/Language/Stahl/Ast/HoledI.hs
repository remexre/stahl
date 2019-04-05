-- |Declarations and expressions as they appear in the source code, with
-- holes, as well as implicits pis, lambdas, and applications.
module Language.Stahl.Ast.HoledI
  ( Decl(..)
  , Expr(..)
  , HoledIExprCustom(..)
  , addImplicitApps
  ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Data.ByteString.UTF8 (ByteString)
import Data.Functor.Const (Const(..))
import Data.Sequence (Seq(..))
import Data.Void (Void)
import Language.Stahl.Ast (GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast as Ast
import qualified Language.Stahl.Ast.Holed as Holed
import Language.Stahl.Env (lookupTy, lookupVal)
import qualified Language.Stahl.Env as Env
import Language.Stahl.Util (Location, convertConstM)

type Decl = Ast.Decl HoledIExprCustom (Const Void) (Maybe Location) (Maybe Location)
type Env = Env.Env HoledIExprCustom (Maybe Location)
type Expr = Ast.Expr HoledIExprCustom (Maybe Location)

data HoledIExprCustom expr
  = Hole ByteString
  | ImplicitApp expr expr
  | ImplicitLam LocalName expr
  | ImplicitPi (Maybe LocalName) expr expr (Seq GlobalName)
  deriving (Functor, Foldable, Show, Traversable)

addImplicitApps :: MonadReader Env m => Holed.Decl -> m Decl
addImplicitApps = Ast.traverseCustomDecl helper convertConstM pure pure
  where helper (Holed.Hole bs) = pure $ Hole bs
        helper (Holed.ImplicitLam n b) = pure $ ImplicitLam n b
        helper (Holed.ImplicitPi n a r es) = pure $ ImplicitPi n a r es
