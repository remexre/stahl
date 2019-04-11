-- |The Stahl typechecker.
--
-- One of the design goals of this iteration of the compiler is for there
-- to only be a single implementation of the core typechecking rules, which
-- is extended in other places where an extended AST is used.
module Language.Stahl.TyCk
  ( TyCkExprAnnot(..)
  , TyCkExprParams(..)
  , UnifVar
  , tyck
  ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Writer (WriterT(..))
import Data.Sequence (Seq(..))
import Language.Stahl.Ast (Expr(..))
import Language.Stahl.Error (Error, ErrorKind(..), mkError)
import Language.Stahl.Internal.Env (Env(..))
import Language.Stahl.Internal.TyCk.Generator (tyckExpr)
import Language.Stahl.Internal.TyCk.Solver (solveConstraints)
import Language.Stahl.Internal.TyCk.Types (TyCkExprAnnot(..), TyCkExprParams(..), UnifVar(..))
import Language.Stahl.Internal.Util.MonadGensym (runGensymT)
import Language.Stahl.Internal.Util.MonadNonfatal (MonadNonfatal(..))
import Language.Stahl.Util (spanBetween')

-- |Type-checks an expression.
tyck :: ( TyCkExprParams c a
        , Show a
        , Show (c (Expr c a))
        , MonadNonfatal Error m
        , MonadReader (Env c a) m
        )
     => Expr c a               -- ^ The expression to typecheck.
     -> Maybe (Expr c a)       -- ^ The type the expression should have.
     -> m (Expr c a, Expr c a) -- ^ Returns the expression and the solved type.
tyck expr chkTy = (uncurry $ solveConstraints expr) =<< (runGensymT . runWriterT $ tyckExpr expr chkTy)

-- |Type-checks an expression, disallowing any actual constraint solving.
tyckWithoutSolving :: ( TyCkExprParams c a
                      , Show a
                      , Show (c (Expr c a))
                      , MonadNonfatal Error m
                      , MonadReader (Env c a) m
                      )
                   => Expr c a               -- ^ The expression to typecheck.
                   -> Maybe (Expr c a)       -- ^ The type the expression should have.
                   -> m (Expr c a, Expr c a) -- ^ Returns the expression and the solved type.
tyckWithoutSolving expr chkTy = helper =<< (runGensymT . runWriterT $ tyckExpr expr chkTy)
  where helper (infTy, Empty) = pure (expr, infTy)
        helper (_, cs) = undefined
