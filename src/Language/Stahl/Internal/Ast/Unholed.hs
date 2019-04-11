-- |Declarations with implicit lambdas and pis tracked, but with holes
-- resolved and implicit applications converted to standard ones. At this
-- stage, built-in values can also come into play.
module Language.Stahl.Internal.Ast.Unholed
  ( Decl(..)
  , Expr(..)
  , ExprCustom(..)
  , solveDeclForHoles
  , solveExprForHoles
  ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Data.Functor.Const (Const(..))
import Data.Sequence (Seq)
import Data.Void (Void, absurd)
import Language.Stahl.Ast (GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast as Ast
import Language.Stahl.Error (Error)
import Language.Stahl.Internal.Ast.Builtins (Builtin(..))
import qualified Language.Stahl.Internal.Ast.HoledI as HoledI
import Language.Stahl.Internal.Util.MonadNonfatal (MonadNonfatal(..))
import Language.Stahl.Util (Location, convertConstM)

type Decl = Ast.Decl ExprCustom (Const Void) (Maybe Location) (Maybe Location)
type Expr = Ast.Expr ExprCustom (Maybe Location)

data ExprCustom expr
  = Builtin Builtin
  | ImplicitLam LocalName expr
  | ImplicitPi (Maybe LocalName) expr expr (Seq GlobalName)
  deriving (Functor, Foldable, Show, Traversable)

solveDeclForHoles :: MonadNonfatal Error m => HoledI.Decl -> m Decl
solveDeclForHoles (Ast.Def n t e a) = do
  t' <- solveExprForHoles t Nothing
  e' <- solveExprForHoles e (Just t')
  pure (Ast.Def n t' e' a)
solveDeclForHoles (Ast.DefTy n k cs a) = error "TODO: solveDeclForHoles DefTy"
solveDeclForHoles (Ast.CustomDecl (Const void) _) = absurd void

solveExprForHoles :: MonadNonfatal Error m => HoledI.Expr -> Maybe Expr -> m Expr
solveExprForHoles = undefined
