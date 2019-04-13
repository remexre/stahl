{-# LANGUAGE Rank2Types, UndecidableInstances #-}

-- |Types used in the typechecker.
module Language.Stahl.Internal.TyCk.Types
  ( Constraint(..)
  , TyCkExprAnnot(..)
  , TyCkExprParams(..)
  , UnifVar
  , freshUnifVar
  , inspectExprVar
  ) where

import Control.Lens (Getter, preview, to)
import Control.Monad ((<=<))
import qualified Data.ByteString.UTF8 as BS
import Data.ByteString.UTF8 (ByteString)
import Data.Default (Default(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Language.Stahl.Ast (Expr(..), exprCustom)
import Language.Stahl.Internal.Util.MonadGensym (MonadGensym(..))
import Language.Stahl.Internal.Util.Value (PP(..))
import Language.Stahl.Internal.Value (Value(..))
import Language.Stahl.Util (Location)

-- |A unification variable.
newtype UnifVar
  = UnifVar Int
  deriving Eq

instance PP UnifVar where
  pp = Symbol Nothing . BS.fromString . show

instance Show UnifVar where
  show (UnifVar n) = "?" <> show n

-- |Creates a fresh 'UnifVar' in a given 'MonadGensym'.
freshUnifVar :: MonadGensym m => m UnifVar
freshUnifVar = UnifVar <$> genInt

-- |Constraints that appear in typechecking.
data Constraint c a
  -- |A constraint that two expressions must be alpha-beta-eta-equivalent.
  --
  --  - Alpha: @(fn (x) x) ~ (fn (y) y)@
  --  - Beta: @((fn (x y) x) (fn (z) z)) ~ (fn (y) (fn (z) z))@
  --  - Eta: @(fn (x) x) ~ (fn (y) (fn (x) x) y)@
  = Expr c a :~: Expr c a

  -- |A constraint that the given expression must by "type-like." This
  -- means it must either be @TYPE@ or a type.
  | TypeLike (Expr c a)

instance (Show a, Show (c (Expr c a))) => Show (Constraint c a) where
  show (l :~: r) = show l <> " ~ " <> show r
  show (TypeLike e) = "TYPELIKE(" <> show e <> ")"

-- |An annotation that can be used for the AST being typechecked.
class TyCkExprAnnot a where
  -- |The default @a@nnotation for an expression created at typechecking
  -- time.
  --
  -- If @a@ is an instance of 'Default', this is automatically implemented.
  defaultAnnot :: a
  default defaultAnnot :: Default a => a
  defaultAnnot = def

  -- |A 'Getter' for the location from which the expression was parsed.
  --
  -- The default implementation just assigns no location to all nodes.
  loc :: Getter a (Maybe Location)
  loc = to (const Nothing)

instance TyCkExprAnnot (Maybe Location) where
  loc = id

-- |Legal parameters for the AST being typechecked.
class (TyCkExprAnnot a, Traversable c) => TyCkExprParams c a where
  -- |Creates a logic variable.
  createVar :: UnifVar -> c (Expr c a)

  -- |Inspects a logic variable.
  inspectVar :: c (Expr c a) -> Maybe (UnifVar, Maybe ByteString)

instance TyCkExprAnnot a => TyCkExprParams (Const UnifVar) a where
  createVar = Const
  inspectVar (Const v) = Just (v, Nothing)

instance (TyCkExprAnnot a, Traversable c) => TyCkExprParams (Compose (Either UnifVar) c) a where
  createVar = Compose . Left
  inspectVar (Compose (Left v)) = Just (v, Nothing)
  inspectVar _ = Nothing

inspectExprVar :: TyCkExprParams c a => Expr c a -> Maybe (UnifVar, Maybe ByteString)
inspectExprVar = inspectVar <=< fmap fst . preview exprCustom
