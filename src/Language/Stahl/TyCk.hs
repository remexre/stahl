{-# LANGUAGE Rank2Types, UndecidableInstances #-}

-- |The Stahl typechecker.
--
-- One of the design goals of this iteration of the compiler is for there
-- to only be a single implementation of the core typechecking rules, which
-- is extended in other places where an extended AST is used.
module Language.Stahl.TyCk
  ( Constraint(..)
  , UnifVar
  , tyck
  , tyckExpr
  ) where

import Control.Lens ((^.), Getter, Prism', review)
import Control.Lens.TH (makeLenses)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Writer (WriterT(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Default (Default(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Language.Stahl.Ast (Expr(..))
import Language.Stahl.Ast.Builtins (Builtin(..))
import Language.Stahl.Env (Env(..), extendEnvWith, extendEnvWith', lookupTy)
import Language.Stahl.Error (Error)
import Language.Stahl.Util (Location)
import Language.Stahl.Util.MonadGensym (MonadGensym(..), runGensymT)
import Language.Stahl.Util.MonadNonfatal (MonadNonfatal(..))

-- |A constraint that two expressions must be alpha-beta-eta-equivalent.
--
--  - Alpha: @(fn (x) x) ~ (fn (y) y)@
--  - Beta: @((fn (x y) x) (fn (z) z)) ~ (fn (y) (fn (z) z))@
--  - Eta: @(fn (x) x) ~ (fn (y) (fn (x) x) y)@
data Constraint c a = (:~:)
  { _l :: Expr c a
  , _r :: Expr c a
  }

instance (Show a, Show (c (Expr c a))) => Show (Constraint c a) where
  show (l :~: r) = show l <> " ~ " <> show r

makeLenses ''Constraint

-- |A unification variable.
newtype UnifVar
  = UnifVar Int
  deriving Eq

instance Show UnifVar where
  show (UnifVar n) = "?" <> show n

-- |Writes a constraint.
(=~=) :: MonadWriter (Seq (Constraint c a)) m => Expr c a -> Expr c a -> m ()
(=~=) l r = tell $ Seq.singleton (l :~: r)

-- |Writes a constraint if the RHS is 'Just'.
(=?=) :: MonadWriter (Seq (Constraint c a)) m => Expr c a -> Maybe (Expr c a) -> m ()
(=?=) l (Just r) = l =~= r
(=?=) l Nothing = pure ()

-- |Type-checks an expression.
tyck :: (Default a, Show a, Show (c (Expr c a)), MonadNonfatal Error m, MonadReader (Env c a) m)
     => Getter a (Maybe Location)     -- ^ A getter for the location information on an AST node.
     -> Prism' (c (Expr c a)) UnifVar -- ^ A prism for creating and inspecting logic variables.
     -> Expr c a                      -- ^ The expression to typecheck.
     -> Maybe (Expr c a)              -- ^ A type to treat as a "hint."
     -> m (Expr c a)                  -- ^ Returns the type of the expression.
tyck loc var expr tyHint = do
  ty <- runGensymT . runWriterT $ tyckExpr loc var expr tyHint
  error $ show ty

-- |The main type-checking function, which produces constraints on the
-- expressions that must be solved later.
tyckExpr :: ( Default a
            , MonadNonfatal Error m
            , MonadGensym m
            , MonadReader (Env c a) m
            , MonadWriter (Seq (Constraint c a)) m
            )
         => Getter a (Maybe Location)     -- ^ A getter for the location information on an AST node.
         -> Prism' (c (Expr c a)) UnifVar -- ^ A prism for creating and inspecting logic variables.
         -> Expr c a                      -- ^ The expression to typecheck.
         -> Maybe (Expr c a)              -- ^ A type to treat as a "hint."
         -> m (Expr c a)                  -- ^ Returns the type of the expression.
tyckExpr loc var (CustomExpr c _)     tyHint = undefined
tyckExpr loc var (App e1 e2 _)        tyHint =
  tyckExpr loc var e1 Nothing >>= \case
    Pi n argT retT es a -> do
      argT' <- tyckExpr loc var e2 (Just argT)
      argT =~= argT'
      -- TODO: Beta reduction!
      -- TODO: Normalize type!
      pure retT
    funT -> do
      argT <- tyckExpr loc var e1 Nothing
      retT <- flip CustomExpr def . review var . UnifVar <$> genInt
      funT =~= Pi Nothing argT retT Seq.empty def
      pure retT
tyckExpr loc var (Atom n _)           tyHint = undefined
tyckExpr loc var (Handle eff e1 e2 _) tyHint = undefined
tyckExpr loc var (Lam n b _)          tyHint = do
  argT <- flip CustomExpr def . review var . UnifVar <$> genInt
  bodyT <- local (extendEnvWith n argT Nothing) $
    tyckExpr loc var b Nothing
  pure (Pi (Just n) argT bodyT Seq.empty def)
tyckExpr loc var (Perform eff b _)    tyHint = undefined
tyckExpr loc var (Pi n t1 t2 effs _)  tyHint = do
  -- TODO: Ensure t1 is typelike
  -- TODO: Ensure t2 is typelike
  pure (Builtin TypeOfTypes def)
tyckExpr loc var (Var n a)            tyHint = do
  ty <- lookupTy n (a^.loc)
  ty =?= tyHint
  pure ty
