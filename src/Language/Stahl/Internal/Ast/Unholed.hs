-- |Declarations with implicit lambdas and pis tracked, but with holes
-- resolved and implicit applications converted to standard ones. At this
-- stage, built-in values can also come into play.
module Language.Stahl.Internal.Ast.Unholed
  ( Decl(..)
  , Expr(..)
  , ExprCustom(..)
  , solveDeclForHoles
  ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Functor.Const (Const(..))
import Data.Sequence (Seq)
import Data.Void (Void)
import Language.Stahl.Ast (GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast as Ast
import Language.Stahl.Error (Error)
import Language.Stahl.Internal.Ast.Builtins (Builtin(..))
import qualified Language.Stahl.Internal.Ast.HoledI as HoledI
import Language.Stahl.Internal.TyCk.Generator (tyckExpr)
import Language.Stahl.Internal.TyCk.Solver (solveConstraints)
import qualified Language.Stahl.Internal.TyCk.Types as TyCk
import Language.Stahl.Internal.Util.MonadGensym (MonadGensym)
import Language.Stahl.Internal.Util.MonadNonfatal (MonadNonfatal(..))
import Language.Stahl.Util (Location, convertConstM)

type Constraint = TyCk.Constraint HoledI.ExprCustom (Maybe Location)
type Decl = Ast.Decl ExprCustom (Const Void) (Maybe Location) (Maybe Location)
type Expr = Ast.Expr ExprCustom (Maybe Location)

data ExprCustom expr
  = Builtin Builtin
  | ImplicitLam LocalName expr
  | ImplicitPi (Maybe LocalName) expr expr (Seq GlobalName)
  deriving (Functor, Foldable, Show, Traversable)

solveDeclForHoles :: ( MonadNonfatal Error m
                     , MonadGensym m
                     , MonadReader HoledI.Env m
                     , MonadWriter (Seq Constraint) m
                     )
                  => HoledI.Decl
                  -> m Decl
solveDeclForHoles decl = do
  undefined

generateDeclConstraints :: ( MonadNonfatal Error m
                           , MonadGensym m
                           , MonadReader HoledI.Env m
                           , MonadWriter (Seq Constraint) m
                           )
                        => HoledI.Expr
                        -> Maybe HoledI.Expr
                        -> m HoledI.Expr
generateDeclConstraints = undefined
