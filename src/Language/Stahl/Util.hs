module Language.Stahl.Util
  ( takeWhileBS
  ) where

import qualified Data.ByteString.UTF8 as BS
import Data.ByteString.UTF8 (ByteString)

takeWhileBS :: (Char -> Bool) -> ByteString -> ByteString
takeWhileBS pred bs = BS.take (len bs) bs
  where len = length . takeWhile pred . BS.toString
