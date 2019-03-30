{-# LANGUAGE DeriveGeneric #-}

module Language.Stahl.Value
  ( Value(..)
  , isSymbolish
  , location
  , symbolishAsNumber
  ) where

import Control.Lens (Lens', lens)
import qualified Data.ByteString as BS (isPrefixOf)
import qualified Data.ByteString.UTF8 as BS
import Data.ByteString.UTF8 (ByteString)
import Data.Char (toLower)
import Data.Int (Int64)
import Data.List (elemIndex)
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

escapeChar :: Char -> String
escapeChar '"' = "\\\""
escapeChar '\\' = "\\\\"
escapeChar c = [c]

isSymbolish :: Char -> Bool
isSymbolish c = inRange c '0' '9' || inRange c 'A' 'Z' || inRange c 'a' 'z' || c `elem` punct
  where inRange n s e = fromEnum s <= fromEnum n && fromEnum n <= fromEnum e
        punct :: String
        punct = "*+-/:<=>?"

symbolishAsNumber :: ByteString -> Maybe Int64
symbolishAsNumber = parseSign . BS.fromString . map toLower . BS.toString
  where charInBase 2 c = fromIntegral <$> (c `elemIndex` ['0', '1'])
        charInBase 10 c = fromIntegral <$> (c `elemIndex` ['0'..'9'])
        charInBase 16 c = fromIntegral <$> (c `elemIndex` (['0'..'9'] <> ['a'..'f']))
        charInBase n c = error ("TODO charInBase " <> show n <> " " <> show c)
        loop base n (BS.uncons -> Just (h, t)) =
          case charInBase base h of
            Just k -> loop base (base * n + k) t
            Nothing -> Nothing
        loop _ n _ = Just n
        parseBase s =
          if "0x" `BS.isPrefixOf` s then
            loop 16 0 (BS.drop 2 s)
          else if "0b" `BS.isPrefixOf` s then
            loop 2 0 (BS.drop 2 s)
          else
            loop 10 0 s
        parseSign s =
          if "+" `BS.isPrefixOf` s then
            parseBase (BS.drop 1 s)
          else if "-" `BS.isPrefixOf` s then
            negate <$> parseBase (BS.drop 1 s)
          else
            parseBase s

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
  show (String _ s) = '"' : (escapeChar =<< BS.toString s) <> "\""
  show (Symbol _ s) = BS.toString s
  show (Nil _) = "()"
