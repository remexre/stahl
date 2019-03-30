module Language.Stahl.Util
  ( Location(..)
  , col
  , colStart
  , colEnd
  , endPoint
  , file
  , line
  , lineStart
  , lineEnd
  , printError
  , startPoint
  , takeWhileBS
  , whenM_
  , writeOrStdout
  ) where

import Control.Lens (Lens', lens)
import Control.Lens.TH (makeLenses)
import Control.Monad (void)
import qualified Data.ByteString.UTF8 as BS
import Data.ByteString.UTF8 (ByteString)
import System.Console.ANSI
  ( Color(Red)
  , ColorIntensity(Vivid)
  , ConsoleLayer(Foreground)
  , SGR(Reset, SetColor)
  , hSetSGR
  , hSupportsANSIColor
  )
import System.IO (hFlush, hPutStr, stderr, stdout)

-- |The location in source code at which an error occurred.
data Location
  = Point
    { _file :: !FilePath
    , _line :: !Word
    , _col :: !Word
    }
  | Span
    { _file :: !FilePath
    , _lineStart :: !Word
    , _colStart :: !Word
    , _lineEnd :: !Word
    , _colEnd :: !Word
    }
  deriving Eq

makeLenses ''Location

instance Show Location where
  show (Point f l c) = f <> ":" <> show l <> ":" <> show c
  show (Span f ls cs le ce) = f <> ":" <> show ls <> ":" <> show cs <> "-" <> show le <> ":" <> show ce

startPoint :: Lens' Location (FilePath, Word, Word)
startPoint = lens get set
  where get (Point f l c) = (f, l, c)
        get (Span f l c _ _) = (f, l, c)
        set (Point _ _ _) (f, l, c) = Point f l c
        set (Span _ _ _ le ce) (f, ls, cs) =  Span f ls cs le ce

endPoint :: Lens' Location (FilePath, Word, Word)
endPoint = lens get set
  where get (Point f l c) = (f, l, c)
        get (Span f l c _ _) = (f, l, c)
        set (Point _ _ _) (f, l, c) = Point f l c
        set (Span _ ls cs _ _) (f, le, ce) =  Span f ls cs le ce

printError :: String -> IO ()
printError err = do
    hFlush stdout
    hFlush stderr
    isatty <- hSupportsANSIColor stderr
    if' isatty $ hSetSGR stderr [SetColor Foreground Vivid Red]
    hPutStr stderr err
    if' isatty $ hSetSGR stderr [Reset]
    hPutStr stderr "\n"
  where if' True b = b
        if' False _ = pure ()

takeWhileBS :: (Char -> Bool) -> ByteString -> ByteString
takeWhileBS pred bs = BS.take (len bs) bs
  where len = length . takeWhile pred . BS.toString

whenM_ :: Monad m => m Bool -> m a -> m ()
whenM_ p body = p >>= \case { True -> void body; False -> pure () }

writeOrStdout :: FilePath -> String -> IO ()
writeOrStdout "-" = putStr
writeOrStdout path = writeFile path
