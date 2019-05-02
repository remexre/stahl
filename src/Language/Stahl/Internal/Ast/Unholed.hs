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
import Control.Monad.Writer (WriterT(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Functor.Const (Const(..))
import Data.Sequence (Seq)
import Data.Validation (Validation, revalidate)
import Data.Void (Void)
import Language.Stahl.Ast (GlobalName(..), LocalName(..), transformDeclCustom, visitDecl)
import qualified Language.Stahl.Ast as Ast
import Language.Stahl.Error (Error)
import Language.Stahl.Internal.Ast.Builtins (Builtin(..))
import qualified Language.Stahl.Internal.Ast.HoledI as HoledI
import Language.Stahl.Internal.TyCk.Generator (tyckExpr)
import Language.Stahl.Internal.TyCk.Solver (solveConstraints)
import qualified Language.Stahl.Internal.TyCk.Types as TyCk
import Language.Stahl.Internal.Util.MonadGensym (MonadGensym)
import Language.Stahl.Internal.Util.MonadNonfatal (MonadNonfatal(..))
import Language.Stahl.Util (Location, constVoid)

type Constraint = TyCk.Constraint HoledI.ExprCustom (Maybe Location)
type Decl = Ast.Decl ExprCustom (Const Void) (Maybe Location) (Maybe Location)
type Expr = Ast.Expr ExprCustom (Maybe Location)

data ExprCustom expr
  = Builtin Builtin
  | ImplicitLam LocalName expr
  | ImplicitPi LocalName expr expr (Seq GlobalName)
  deriving (Functor, Foldable, Show, Traversable)

solveDeclForHoles :: (MonadGensym m, MonadNonfatal Error m, MonadReader HoledI.Env m)
                  => HoledI.Decl -> m Decl
solveDeclForHoles = transformDeclCustom undefined constVoid

generateDeclConstraints :: ( MonadGensym m
                           , MonadNonfatal Error m
                           , MonadReader HoledI.Env m
                           , MonadWriter (Seq Constraint) m
                           )
                        => HoledI.Expr
                        -> Maybe HoledI.Expr
                        -> m HoledI.Expr
generateDeclConstraints = undefined

findHoles' :: HoledI.Decl -> Validation [Error] ()
findHoles' = visitDecl helper (const $ pure ())
  where helper e@(Ast.CustomExpr (HoledI.Hole _ _) _) = undefined
        helper _ = pure ()

removeHoles :: (MonadGensym m, MonadNonfatal Error m, MonadReader HoledI.Env m)
            => HoledI.Decl -> m Decl
removeHoles = transformDeclCustom helperE constVoid
  where helperE (HoledI.Hole n v) loc = undefined
        helperE (HoledI.ImplicitApp f x) loc = undefined
        helperE (HoledI.ImplicitLam n b) loc = undefined
        helperE (HoledI.ImplicitPi n t1 t2 es) loc = undefined
