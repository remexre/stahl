module Language.Stahl
  ( Error(..)
  , ErrorKind(..)
  , Location(..)
  , ToError(..)
  , chain
  , parseFile
  ) where

import Language.Stahl.Error
  ( Error(..)
  , ErrorKind(..)
  , Location(..)
  , ToError(..)
  , chain
  )

parseFile :: FilePath -> ()
parseFile path = undefined
