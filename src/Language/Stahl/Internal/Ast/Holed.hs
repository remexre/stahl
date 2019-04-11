-- |Declarations and expressions as they appear in the source code, with
-- implicits pis and lambdas, as well as holes, but without implicit
-- applications expanded (see 'Language.Stahl.Ast.HoledI' for that).
module Language.Stahl.Internal.Ast.Holed
  ( Decl(..)
  , Expr(..)
  , ExprCustom(..)
  , declsFromValues
  , exprFromValue
  ) where

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as BS
import Data.Functor.Const (Const(..))
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import Language.Stahl.Ast (GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast as Ast
import Language.Stahl.Error (Error, astError)
import Language.Stahl.Internal.Util (Location)
import Language.Stahl.Internal.Util.MonadNonfatal (MonadNonfatal(..), mapFatalsToNonfatals)
import Language.Stahl.Internal.Util.Value (PP(..), valueAsList)
import Language.Stahl.Internal.Value (Value(..))

type Decl = Ast.Decl ExprCustom (Const Void) (Maybe Location) (Maybe Location)
type Expr = Ast.Expr ExprCustom (Maybe Location)

data ExprCustom expr
  = Hole ByteString
  | ImplicitLam LocalName expr
  | ImplicitPi (Maybe LocalName) expr expr (Seq GlobalName)
  deriving (Eq, Functor, Foldable, Show, Traversable)

instance PP (ExprCustom (Ast.Expr ExprCustom a)) where
  pp (Hole s) = Symbol Nothing ("_" <> s)
  pp (ImplicitLam n e) = helper [n] e
    where helper ns (Ast.CustomExpr (ImplicitLam n' e') _) = helper (n':ns) e'
          helper ns e = pp [Symbol Nothing "fn*", pp (pp <$> reverse ns), pp e]
  pp (ImplicitPi (Just n) t1 t2 Empty) = pp [Symbol Nothing "pi*", pp n, pp t1, pp t2]
  pp (ImplicitPi Nothing t1 t2 Empty)  = helper [t1] t2
    where helper t1s (Ast.CustomExpr (ImplicitPi Nothing t1' t2' Empty) _) = helper (t1':t1s) t2'
          helper t1s e = pp [Symbol Nothing "fun*", pp (pp <$> reverse t1s), pp e]
  pp (ImplicitPi (Just n) t1 t2 effs)  = pp [Symbol Nothing "pi*", pp n, pp t1, pp t2, pp effs]
  pp (ImplicitPi Nothing t1 t2 effs)   = pp [Symbol Nothing "pi*", Symbol Nothing "_", pp t1, pp t2, pp effs]

declsFromValues :: MonadNonfatal Error m => [Value] -> m (Seq Decl)
declsFromValues = fmap Seq.fromList . mapFatalsToNonfatals declFromValue

declFromValue :: MonadNonfatal Error m => Value -> m Decl
declFromValue v = uncurry (declFromValue' v) =<< valueAsList (astError "declaration") v

declFromValue' :: MonadNonfatal Error m => Value -> Maybe Location -> [Value] -> m Decl

declFromValue' _ loc [Symbol _ "def", Symbol _ name, expr] =
  Ast.Def (LocalName name) <$> pure (Ast.CustomExpr (Hole ("_type_of_" <> name)) Nothing)
                           <*> exprFromValue expr
                           <*> pure loc
declFromValue' _ loc [Symbol _ "def", Symbol _ name, ty, expr] =
  Ast.Def (LocalName name) <$> exprFromValue ty <*> exprFromValue expr <*> pure loc
declFromValue' val loc (Symbol _ "def":_) = fatal (astError "def" val)

declFromValue' _ loc (Symbol _ "defty":Symbol _ name:kind:ctors) =
  Ast.DefTy (LocalName name) <$> exprFromValue kind
                           <*> mapM ctorFromValue (Seq.fromList ctors)
                           <*> pure loc
declFromValue' val loc (Symbol _ "defty":_) = fatal (astError "defty" val)

declFromValue' val loc _ = fatal (astError "declaration" val)

ctorFromValue :: MonadNonfatal Error m => Value -> m (LocalName, Expr)
ctorFromValue v = valueAsList (astError "constructor") v >>= \case
  (loc, [Symbol _ name, ty]) -> (LocalName name,) <$> exprFromValue ty
  _ -> fatal (astError "constructor" v)

exprFromValue :: MonadNonfatal Error m => Value -> m Expr
exprFromValue v@(Symbol loc name) =
  case BS.uncons name of
    Just ('_', rest) -> pure (Ast.CustomExpr (Hole rest) loc)
    Just _ -> pure (Ast.Var (LocalName name) loc)
    Nothing -> fatal (astError "variable" v)
exprFromValue v = valueAsList (astError "expression") v >>= \case
  (loc, l@(_:_)) -> appExprsFromExprs loc =<< mapM exprFromValue l
  _ -> fatal (astError "expression" v)

-- Converts a list of expressions into 'App' expressions.
appExprsFromExprs :: MonadNonfatal Error m => Maybe Location -> [Expr] -> m Expr
appExprsFromExprs _ [] = error "unreachable"
appExprsFromExprs _ [x] = pure x
appExprsFromExprs loc l@(_:_) = Ast.App <$> appExprsFromExprs loc (init l) <*> pure (last l) <*> pure loc
