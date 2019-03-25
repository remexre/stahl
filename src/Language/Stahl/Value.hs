{-# LANGUAGE DeriveGeneric #-}

module Language.Stahl.Value
  ( Value(..)
  , isSymbolish
  , location
  ) where

import Control.Lens (Lens', lens)
import qualified Data.ByteString.UTF8 as BS
import Data.ByteString.UTF8 (ByteString)
import Data.Int (Int64)
import GHC.Generics (Generic)
import Language.Stahl.Util (Location(..))

-- |A parsed value.
data Value
  = Cons   !Location !Value !Value
  | Int    !Location !Int64
  | String !Location !ByteString
  | Symbol !Location !ByteString
  | Nil    !Location
  deriving Generic

escapeChar :: Bool -> Char -> String
escapeChar True '"' = "\\\""
escapeChar False '}' = "\\}"
escapeChar _ c = [c]

isSymbolish :: Char -> Bool
isSymbolish c = inRange c '0' '9' || inRange c 'A' 'Z' || inRange c 'a' 'z' || c `elem` punct
  where inRange n s e = fromEnum s <= fromEnum n && fromEnum n <= fromEnum e
        punct :: String
        punct = "*+-/:<=>?"

location :: Lens' Value Location
location = lens get set
  where get (Cons l _ _) = l
        get (Int l _)    = l
        get (String l _) = l
        get (Symbol l _) = l
        get (Nil l)      = l
        set (Cons _ h t) l = Cons   l h t
        set (Int _ n)    l = Int    l n
        set (String _ n) l = String l n
        set (Symbol _ n) l = Symbol l n
        set (Nil _)      l = Nil    l

showTail :: Value -> String
showTail (Cons _ h t) = ' ' : show h <> showTail t
showTail (Nil _) = ")"
showTail v = " | " <> show v <> ")"

instance Eq Value where
  (Cons _ hl tl) == (Cons _ hr tr) = (hl == hr) && (tl == tr)
  (Int _ l)      == (Int _ r)      = l == r
  (String _ l)   == (String _ r)   = l == r
  (Symbol _ l)   == (Symbol _ r)   = l == r
  (Nil _)        == (Nil _)        = True
  _ == _ = False

instance Show Value where
  show (Cons _ h t) = '(' : show h <> showTail t
  show (Int _ n) = show n
  show (String _ s) = '"' : (escapeChar True =<< BS.toString s) <> "\""
  show (Symbol _ s) = if all isSymbolish s' && not (null s') then s' else '{' : (escapeChar False =<< s') <> "}"
    where s' = BS.toString s
  show (Nil _) = "()"
