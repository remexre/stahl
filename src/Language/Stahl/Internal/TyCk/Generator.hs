-- |The constraint-generating parts of the typechecker.
module Language.Stahl.Internal.TyCk.Generator
  ( tyckExpr
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Language.Stahl.Ast (Expr(..), LocalName(..))
import Language.Stahl.Error (Error, ErrorKind(..), mkError)
import Language.Stahl.Internal.Ast.Builtins (Builtin(..))
import Language.Stahl.Internal.Env (Env(..), extendEnvWith, lookupTy)
import Language.Stahl.Internal.TyCk.Types
  ( Constraint(..)
  , TyCkExprAnnot(..)
  , TyCkExprParams(..)
  , freshUnifVar
  )
import Language.Stahl.Internal.Util.MonadGensym (MonadGensym(..))
import Language.Stahl.Internal.Util.MonadNonfatal (MonadNonfatal(..))

-- |Writes an equality constraint.
(=~=) :: MonadWriter (Seq (Constraint c a)) m => Expr c a -> Expr c a -> m ()
(=~=) l r = constrain (l :~: r)

-- |Writes a constraint.
constrain :: MonadWriter (Seq (Constraint c a)) m => Constraint c a -> m ()
constrain = tell . Seq.singleton

-- |A wrapper around 'tyckExpr'' that enforces the check-type.
tyckExpr :: ( TyCkExprParams c a
            , MonadGensym m
            , MonadNonfatal Error m
            , MonadReader (Env c a) m
            , MonadWriter (Seq (Constraint c a)) m
            )
         => Expr c a         -- ^ The expression to typecheck.
         -> Maybe (Expr c a) -- ^ The type the expression should have.
         -> m (Expr c a)     -- ^ Returns the type of the expression.
tyckExpr expr chkTy = do
  infTy <- tyckExpr' expr
  case chkTy of
    Just chkTy' -> do
      infTy =~= chkTy'
      -- This is arguably a semi-controversial choice, but it should reduce
      -- the number of type errors to the "real ones."
      pure chkTy'
    Nothing -> pure infTy

-- |The main type-checking function, which produces constraints on the
-- expressions that must be solved later.
tyckExpr' :: ( TyCkExprParams c a
             , MonadGensym m
             , MonadNonfatal Error m
             , MonadReader (Env c a) m
             , MonadWriter (Seq (Constraint c a)) m
             )
          => Expr c a     -- ^ The expression to typecheck.
          -> m (Expr c a) -- ^ Returns the type of the expression.
tyckExpr' (CustomExpr c _) = undefined
tyckExpr' (App e1 e2 _) =
  tyckExpr e1 Nothing >>= \case
    Pi n argT retT es a -> do
      argT' <- tyckExpr e2 (Just argT)
      argT =~= argT'
      -- TODO: Beta reduction!
      -- TODO: Normalize type!
      pure retT
    funT -> do
      argT <- tyckExpr e1 Nothing
      retT <- flip CustomExpr defaultAnnot . createVar <$> freshUnifVar
      newName <- genSym
      funT =~= Pi (LocalName newName, True) argT retT Seq.empty defaultAnnot
      pure retT
tyckExpr' (Atom n _) = undefined
tyckExpr' (Builtin TypeOfTypes _) =
  pure (Builtin TypeOfTypeOfTypes defaultAnnot)
tyckExpr' (Builtin TypeOfTypeOfTypes annot) =
  fatal (mkError (Other "The type of the type of TYPE does not exist") (annot^.loc))
tyckExpr' (Handle eff e1 e2 _) = undefined
tyckExpr' (Lam (n, isGensym) b _) = do
  argT <- flip CustomExpr defaultAnnot . createVar <$> freshUnifVar
  bodyT <- local (extendEnvWith n argT Nothing) $
    tyckExpr b Nothing
  pure (Pi (n, isGensym) argT bodyT Seq.empty defaultAnnot)
tyckExpr' (Perform eff b _) = undefined
tyckExpr' (Pi n t1 t2 effs _) = do
  constrain (TypeLike t1)
  constrain (TypeLike t2)
  pure (Builtin TypeOfTypes defaultAnnot)
tyckExpr' (Var n a) = lookupTy n (a^.loc)
