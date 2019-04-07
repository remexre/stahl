{-# LANGUAGE MultiParamTypeClasses, Rank2Types, UndecidableInstances #-}

-- |Types used in the typechecker.
module Language.Stahl.TyCk.Types
  ( Constraint(..)
  , TyCkExprAnnot(..)
  , TyCkExprParams(..)
  , UnifVar
  , freshUnifVar
  ) where

import Control.Lens (_Left, Getter, Prism', to)
import Data.Default (Default(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Language.Stahl.Ast (Expr(..))
import Language.Stahl.Util (Location, _Compose, _Const)
import Language.Stahl.Util.MonadGensym (MonadGensym(..))

-- |A unification variable.
newtype UnifVar
  = UnifVar Int
  deriving Eq

instance Show UnifVar where
  show (UnifVar n) = "?" <> show n

freshUnifVar :: MonadGensym m => m UnifVar
freshUnifVar = undefined

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
class TyCkExprAnnot a => TyCkExprParams c a where
  -- |A 'Prism' for creating and inspecting logic variables.
  var :: Prism' (c (Expr c a)) UnifVar

instance TyCkExprAnnot a => TyCkExprParams (Const UnifVar) a where
  var = _Const

instance TyCkExprAnnot a => TyCkExprParams (Compose (Either UnifVar) c) a where
  var = _Compose._Left
