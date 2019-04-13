-- |Declarations and expressions as they appear in the source code, with
-- holes, as well as implicits pis, lambdas, and applications.
module Language.Stahl.Internal.Ast.HoledI
  ( Decl(..)
  , Expr(..)
  , ExprCustom(..)
  , addImplicitApps
  , addImplicitApps'
  ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Data.ByteString.UTF8 (ByteString)
import Data.Functor.Const (Const(..))
import Data.Sequence (Seq(..))
import Data.Void (Void)
import Language.Stahl.Ast (GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast as Ast
import qualified Language.Stahl.Internal.Env as Env
import qualified Language.Stahl.Internal.Ast.Holed as Holed
import Language.Stahl.Internal.Env (lookupTy, lookupVal)
import Language.Stahl.Internal.TyCk.Types (TyCkExprAnnot, TyCkExprParams(..), UnifVar, freshUnifVar)
import Language.Stahl.Internal.Util.MonadGensym (MonadGensym)
import Language.Stahl.Util (Location, convertConstM)

type Decl = Ast.Decl ExprCustom (Const Void) (Maybe Location) (Maybe Location)
type Env = Env.Env ExprCustom (Maybe Location)
type Expr = Ast.Expr ExprCustom (Maybe Location)

data ExprCustom expr
  = Hole ByteString UnifVar
  | ImplicitApp expr expr
  | ImplicitLam LocalName expr
  | ImplicitPi (Maybe LocalName) expr expr (Seq GlobalName)
  deriving (Functor, Foldable, Show, Traversable)

instance TyCkExprAnnot a => TyCkExprParams ExprCustom a where
  createVar = Hole "_"
  inspectVar (Hole n v) = Just (v, Just n)
  inspectVar _ = Nothing

addImplicitApps :: (MonadGensym m, MonadReader Env m) => Holed.Decl -> m Decl
addImplicitApps = Ast.traverseCustomDecl helper convertConstM pure pure
  where helper (Holed.GlobalVar n) = error "TODO"
        helper (Holed.Hole bs) = Hole bs <$> freshUnifVar
        helper (Holed.ImplicitLam n b) = pure $ ImplicitLam n b
        helper (Holed.ImplicitPi n a r es) = pure $ ImplicitPi n a r es

addImplicitApps' :: (MonadGensym m, MonadReader Env m) => Holed.Expr -> m Expr
addImplicitApps' = undefined
