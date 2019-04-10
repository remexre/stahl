module Language.Stahl.Internal.Util
  ( Location(..)
  , col
  , _Compose
  , _Const
  , colStart
  , colEnd
  , convertConst
  , convertConstM
  , endPoint
  , file
  , line
  , lineStart
  , lineEnd
  , printError
  , repeatM
  , spanBetween
  , spanBetween'
  , startPoint
  , takeWhileBS
  , whenM_
  , wholeFile
  , writeOrStdout
  ) where

import Control.Lens (Iso', Lens', (^.), iso, lens)
import Control.Lens.TH (makeLenses)
import Control.Monad (void)
import qualified Data.ByteString.UTF8 as BS
import Data.Bifunctor (Bifunctor(..))
import Data.ByteString.UTF8 (ByteString)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
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

-- |Creates a span between the location(s) provided.
spanBetween :: Maybe Location -> Location -> Location
spanBetween (Just start) end = Span f ls cs le ce
  where (f, ls, cs) = start^.startPoint
        (_, le, ce) = end^.endPoint
spanBetween Nothing end = end

-- |Creates a span between the location(s) provided.
spanBetween' :: Maybe Location -> Maybe Location -> Maybe Location
spanBetween' (Just start) (Just end) = Just $ Span f ls cs le ce
  where (f, ls, cs) = start^.startPoint
        (_, le, ce) = end^.endPoint
spanBetween' start Nothing = start
spanBetween' Nothing end = end

-- |Returns a 'Location' for the entire file.
wholeFile :: FilePath -> ByteString -> Location
wholeFile path src = Span path 0 0 l c
  where (l, c) = second (fromIntegral . BS.length) $ lastOr ([1..] `zip` BS.lines src) (0, "")
        lastOr [] y = y
        lastOr [x] _ = x
        lastOr (h:t) y = lastOr t y

-- |An 'Iso'' between a 'Compose functor and the value inside it.
_Compose :: Iso' (Compose f g x) (f (g x))
_Compose = iso getCompose Compose

-- |An 'Iso'' between a 'Const' functor and the value inside it.
_Const :: Iso' (Const a b) a
_Const = iso getConst Const

convertConst :: Const a b -> Const a c
convertConst (Const x) = Const x

convertConstM :: Applicative f => Const a b -> f (Const a c)
convertConstM = pure . convertConst

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

repeatM :: Monad m => m Bool -> m ()
repeatM act = act >>= \case { True -> repeatM act; False -> pure () }

takeWhileBS :: (Char -> Bool) -> ByteString -> ByteString
takeWhileBS pred bs = BS.take (len bs) bs
  where len = length . takeWhile pred . BS.toString

whenM_ :: Monad m => m Bool -> m a -> m ()
whenM_ p body = p >>= \case { True -> void body; False -> pure () }

writeOrStdout :: FilePath -> String -> IO ()
writeOrStdout "-" = putStr
writeOrStdout path = writeFile path
