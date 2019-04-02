module Language.Stahl.Ast.Holed
  ( Annot(..)
  , Decl(..)
  , Expr(..)
  , GlobalName(..)
  , LocalName(..)
  , declsFromValues
  ) where

import Control.Lens ((^.))
import Data.ByteString.UTF8 (ByteString)
import Data.Functor.Const (Const(..))
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import Language.Stahl.Ast.Generic (Annot(..), GlobalName(..), LocalName(..))
import qualified Language.Stahl.Ast.Generic as G
import Language.Stahl.Error (Error(..), ErrorKind(..), astError)
import Language.Stahl.Util (Location)
import Language.Stahl.Util.MonadNonfatal (MonadNonfatal(..), mapFatalsToNonfatals, runNonfatal)
import Language.Stahl.Util.Value (valueAsList, valueAsSHL)
import Language.Stahl.Value (Value(..), location)

type Decl aE aD = G.Decl ExprCustom (Const Void) aE aD
type Expr a = G.Expr ExprCustom a

data ExprCustom expr
  = Hole ByteString
  | ImplicitLam LocalName expr
  | ImplicitPi (Maybe LocalName) expr expr (Seq GlobalName)

declsFromValues :: [Value] -> ([Error], Seq (Decl (Maybe Location) (Maybe Location)))
declsFromValues = foldr helper ([], Seq.empty) . map (runNonfatal . declFromValue)
  where helper (Left es') (es, ds) = (es <> es', ds)
        helper (Right d) (es, ds) = (es, ds |> d)

declFromValue :: MonadNonfatal Error m => Value -> m (Decl (Maybe Location) (Maybe Location))
declFromValue v = uncurry (declFromValue' v) =<< valueAsList (astError "declaration") v

declFromValue' :: MonadNonfatal Error m => Value -> Location -> [Value] -> m (Decl (Maybe Location) (Maybe Location))
declFromValue' _ loc [Symbol _ "def", Symbol _ name, expr] =
  G.Def (LocalName name) (G.CustomExpr (Hole ("_type_of_" <> name)) Nothing) <$> exprFromValue expr <*> pure (Just loc)
declFromValue' _ loc [Symbol _ "def", Symbol _ name, ty, expr] =
  G.Def (LocalName name) <$> exprFromValue ty <*> exprFromValue expr <*> pure (Just loc)
declFromValue' val loc (Symbol _ "def":_) = fatal (astError "def" val)
declFromValue' val loc _ = fatal (astError "declaration" val)

exprFromValue :: MonadNonfatal Error m => Value -> m (Expr (Maybe Location))
exprFromValue v = uncurry (exprFromValue' v) =<< valueAsList (astError "expression") v

exprFromValue' :: MonadNonfatal Error m => Value -> Location -> [Value] -> m (Expr (Maybe Location))
exprFromValue' val loc _ = fatal (astError "expression" val)
