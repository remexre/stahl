-- |A dependently typed Lisp with algebraic effects.
module Language.Stahl
  ( Builtin(..)
  , Decl(..)
  , Error(..)
  , ErrorKind(..)
  , Expr(..)
  , GlobalName(..)
  , LocalName(..)
  , Library(..)
  , LibMeta(..)
  , LibName(..)
  , Location(..)
  , Module(..)
  , Value(..)
  , loadLibrary
  , parse
  , parseFile
  , tyck
  ) where

import Language.Stahl.Ast (Decl(..), Expr(..), GlobalName(..), LocalName(..))
import Language.Stahl.Ast.Builtins (Builtin(..))
import Language.Stahl.Error (Error(..), ErrorKind(..))
import Language.Stahl.Parser (parse, parseFile)
import Language.Stahl.Modules (Library(..), LibMeta(..), LibName(..), Module(..), loadLibrary)
import Language.Stahl.TyCk (tyck)
import Language.Stahl.Util (Location(..))
import Language.Stahl.Value (Value(..))
