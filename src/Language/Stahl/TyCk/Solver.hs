{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}

-- |The constraint-solving parts of the typechecker.
module Language.Stahl.TyCk.Solver
  ( solveConstraints
  ) where

import Control.Lens ((.=), preview, to, use)
import Control.Lens.TH (makeLenses)
import Control.Monad.State (StateT, execStateT)
import Data.List (sortOn)
import Data.Sequence (Seq)
import Language.Stahl.Ast (Expr(..), custom)
import Language.Stahl.Ast.Builtins (Builtin(..))
import Language.Stahl.Error (Error)
import Language.Stahl.TyCk.Types (Constraint(..), TyCkExprParams(..))
import Language.Stahl.Util (repeatM)
import Language.Stahl.Util.MonadNonfatal (MonadNonfatal(..))

-- |The internal state of the solver.
data SolverState c a = SolverState
  { _constraints :: [Constraint c a]
  , _expr :: Expr c a
  , _infTy :: Expr c a
  }

deriving instance (Show a, Show (c (Expr c a))) => Show (SolverState c a)

makeLenses ''SolverState

solveConstraints :: (TyCkExprParams c a, Show a, Show (c (Expr c a)), MonadNonfatal Error m)
                 => Expr c a               -- ^ The expression to typecheck.
                 -> Expr c a               -- ^ The inferred type of the expression.
                 -> Seq (Constraint c a)   -- ^ The constraints that must be solved.
                 -> m (Expr c a, Expr c a) -- ^ Returns the expression and the solved type.
solveConstraints expr _ _ = undefined <$> execStateT solveConstraints' state
  where state = SolverState undefined expr undefined

solveConstraints' :: (MonadNonfatal Error m, Show a, Show (c (Expr c a)), TyCkExprParams c a)
                  => StateT (SolverState c a) m ()
solveConstraints' = repeatM $ do
  let constraintPriority (_ :~: _) = 0
      constraintPriority (TypeLike _) = 1
  use (constraints . to (sortOn constraintPriority)) >>= \case
    [] -> pure False
    (hd:tl) -> do
      constraints .= tl
      solveConstraint hd
      pure True

solveConstraint :: (MonadNonfatal Error m, Show a, Show (c (Expr c a)), TyCkExprParams c a)
                => Constraint c a -> StateT (SolverState c a) m ()
solveConstraint ((preview (custom.to fst.var) -> l) :~: r) = undefined
solveConstraint (TypeLike (Builtin TypeOfTypes _)) = pure ()
solveConstraint (TypeLike expr) = error ("TODO: tyckExpr `" <> show expr <> "` `TYPE`")
