{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}

-- |The constraint-solving parts of the typechecker.
module Language.Stahl.TyCk.Solver
  ( solveConstraints
  ) where

import Control.Lens (Traversal', (^.), (.=), preview, to, use, view)
import Control.Lens.TH (makeLenses)
import Control.Monad.State (StateT, execStateT)
import Control.Monad.State.Class (modify)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity(..))
import Data.List (sortOn)
import Data.Sequence (Seq)
import Language.Stahl.Ast (Expr(..), mapCustomExpr)
import Language.Stahl.Ast.Builtins (Builtin(..))
import Language.Stahl.Error (Error)
import Language.Stahl.TyCk.Types (Constraint(..), TyCkExprParams(..), UnifVar, var')
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

traverseConstraint :: Traversal' (Constraint c a) (Expr c a)
traverseConstraint f (l :~: r) = (:~:) <$> f l <*> f r
traverseConstraint f (TypeLike e) = TypeLike <$> f e

traverseSolverState :: Traversal' (SolverState c a) (Expr c a)
traverseSolverState f s = SolverState <$> traverse (traverseConstraint f) (s^.constraints)
                                      <*> f (s^.expr)
                                      <*> f (s^.infTy)

solveConstraints :: (TyCkExprParams c a, Show a, Show (c (Expr c a)), MonadNonfatal Error m)
                 => Expr c a               -- ^ The expression to typecheck.
                 -> Expr c a               -- ^ The inferred type of the expression.
                 -> Seq (Constraint c a)   -- ^ The constraints that must be solved.
                 -> m (Expr c a, Expr c a) -- ^ Returns the expression and the solved type.
solveConstraints expr' infTy' cs = helper <$> execStateT solveConstraints' state
  where helper s = (s^.expr, s^.infTy)
        state = SolverState (toList cs) expr' infTy'

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
solveConstraint ((view var' -> Just l) :~: r) = modify $ replace l r
solveConstraint (l :~: (view var' -> Just r)) = modify $ replace r l
solveConstraint c@(_ :~: _) = error ("TODO: solveConstraint " <> show c)
solveConstraint (TypeLike (Builtin TypeOfTypes _)) = pure ()
solveConstraint (TypeLike expr) = error ("TODO: tyckExpr `" <> show expr <> "` `TYPE`")

class Replaceable t where
  replace :: UnifVar -> Expr c a -> t -> t

instance TyCkExprParams c a => Replaceable (Expr c a) where
  replace from to = mapCustomExpr helper id
    where helper :: c (Expr c a) -> c (Expr c a)
          helper ex@(preview var -> Just x)
            | x == from = undefined to
            | otherwise = ex
          helper ex = ex

instance TyCkExprParams c a => Replaceable (Constraint c a) where
  replace from to = runIdentity . traverseConstraint (Identity . replace from to)

instance TyCkExprParams c a => Replaceable (SolverState c a) where
  replace from to = runIdentity . traverseSolverState (Identity . replace from to)
