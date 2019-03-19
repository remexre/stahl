module Language.Stahl.Value
  ( Value(..)
  ) where

import qualified Data.ByteString.UTF8 as BS
import Data.ByteString.UTF8 (ByteString)
import Data.Int (Int64)
import Language.Stahl.Error (Location(..))

-- |A parsed value.
data Value
  = Cons   !Location !Value !Value
  | Int    !Location !Int64
  | String !Location !ByteString
  | Symbol !Location !ByteString
  | Nil    !Location

escapeChar :: Char -> String
escapeChar c = [c]

showTail :: Value -> String
showTail (Cons _ h t) = ' ' : show h <> showTail t
showTail (Nil _) = ")"
showTail v = " | " <> show v <> ")"

instance Show Value where
  show (Cons _ h t) = '(' : show h <> showTail t
  show (Int _ n) = show n
  show (String _ s) = '"' : (escapeChar =<< BS.toString s) <> "\""
  show (Symbol _ s) = show s
  show (Nil _) = "()"
