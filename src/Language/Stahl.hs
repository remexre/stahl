-- |A dependently typed Lisp with algebraic effects.
module Language.Stahl
  ( Error(..)
  , ErrorKind(..)
  , Location(..)
  , Value(..)
  , parse
  , parseFile
  ) where

import Language.Stahl.Error
  ( Error(..)
  , ErrorKind(..)
  , Location(..)
  )
import Language.Stahl.Parser (parse, parseFile)
import Language.Stahl.Value (Value(..))
