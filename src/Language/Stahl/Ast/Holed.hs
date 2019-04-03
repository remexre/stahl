module Language.Stahl.Ast.Holed
  ( Decl(..)
  , Expr(..)
  , HoledExprCustom(..)
  , declsFromValues
  ) where

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as BS
import Data.Functor.Const (Const(..))
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import Language.Stahl.Ast.Generic (GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast.Generic as G
import Language.Stahl.Error (Error(..), astError)
import Language.Stahl.Util (Location)
import Language.Stahl.Util.MonadNonfatal (MonadNonfatal(..), mapFatalsToNonfatals)
import Language.Stahl.Util.Value (valueAsList)
import Language.Stahl.Value (Value(..))

type Decl = G.Decl HoledExprCustom (Const Void) (Maybe Location) (Maybe Location)
type Expr = G.Expr HoledExprCustom (Maybe Location)

data HoledExprCustom expr
  = Hole ByteString
  | ImplicitLam LocalName expr
  | ImplicitPi (Maybe LocalName) expr expr (Seq GlobalName)

declsFromValues :: MonadNonfatal Error m => [Value] -> m (Seq Decl)
declsFromValues = fmap Seq.fromList . mapFatalsToNonfatals declFromValue

declFromValue :: MonadNonfatal Error m => Value -> m Decl
declFromValue v = uncurry (declFromValue' v) =<< valueAsList (astError "declaration") v

declFromValue' :: MonadNonfatal Error m => Value -> Location -> [Value] -> m Decl

declFromValue' _ loc [Symbol _ "def", Symbol _ name, expr] =
  G.Def (LocalName name) <$> pure (G.CustomExpr (Hole ("_type_of_" <> name)) Nothing)
                         <*> exprFromValue expr
                         <*> pure (Just loc)
declFromValue' _ loc [Symbol _ "def", Symbol _ name, ty, expr] =
  G.Def (LocalName name) <$> exprFromValue ty <*> exprFromValue expr <*> pure (Just loc)
declFromValue' val loc (Symbol _ "def":_) = fatal (astError "def" val)

declFromValue' _ loc (Symbol _ "defty":Symbol _ name:kind:ctors) =
  G.DefTy (LocalName name) <$> exprFromValue kind
                           <*> mapM ctorFromValue (Seq.fromList ctors)
                           <*> pure (Just loc)
declFromValue' val loc (Symbol _ "defty":_) = fatal (astError "defty" val)

declFromValue' val loc _ = fatal (astError "declaration" val)

ctorFromValue :: MonadNonfatal Error m => Value -> m (LocalName, Expr)
ctorFromValue v = valueAsList (astError "constructor") v >>= \case
  (loc, [Symbol _ name, ty]) -> (LocalName name,) <$> exprFromValue ty
  _ -> fatal (astError "constructor" v)

exprFromValue :: MonadNonfatal Error m => Value -> m Expr
exprFromValue v@(Symbol loc name) =
  case BS.uncons name of
    Just ('_', rest) -> pure (G.CustomExpr (Hole rest) (Just loc))
    Just _ -> pure (G.Var (LocalName name) (Just loc))
    Nothing -> fatal (astError "variable" v)
exprFromValue v = valueAsList (astError "expression") v >>= \case
  (loc, l@(_:_)) -> appExprsFromExprs loc =<< mapM exprFromValue l
  _ -> fatal (astError "expression" v)

-- Converts a list of expressions into 'App' expressions.
appExprsFromExprs :: MonadNonfatal Error m => Location -> [Expr] -> m Expr
appExprsFromExprs _ [] = error "unreachable"
appExprsFromExprs _ [x] = pure x
appExprsFromExprs loc l@(_:_) = G.App <$> appExprsFromExprs loc (init l) <*> pure (last l) <*> pure (Just loc)
