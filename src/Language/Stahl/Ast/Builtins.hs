module Language.Stahl.Ast.Builtins
  ( Builtin(..)
  ) where

-- |A value known to the compiler, and handled specially (mainly for
-- typechecking).
data Builtin
  = TypeOfTypes
  | TypeOfTypeOfTypes
  deriving (Eq, Show)
