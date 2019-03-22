{-# LANGUAGE DeriveFunctor, RankNTypes #-}

module Language.Stahl.Lexer
  ( LexerError(..)
  , LexerState
  , Token(..)
  , lexer
  , mkLexerState
  ) where

import Control.Arrow (second)
import Control.Lens (ReifiedLens(..), ReifiedLens', (.=), (<>=), assign, use)
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (ExceptT(..), MonadError(..), liftEither, runExceptT)
import Control.Monad.Loops (whileM_)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.State.Strict (MonadState(..), StateT(..))
import qualified Data.ByteString as BS (null)
import qualified Data.ByteString.UTF8 as BS
import Data.ByteString.UTF8 (ByteString)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word)
import Debug.Trace (trace)
import Language.Stahl.Error (Error, ErrorKind(..), Location(..), ToError(..))

data Point = P Word Word deriving (Eq, Show)
data Span = S Point Point deriving (Eq, Show)

data Token a
  = TokEOF a
  | TokDedent a
  | TokGroup a
  | TokIndent a
  | TokNewline a
  | TokParenClose a
  | TokParenOpen a
  | TokPipe a
  | TokInt (Int64, a)
  | TokString (ByteString, a)
  | TokSymbol (ByteString, a)
  deriving (Functor, Show)

data LexerError
  = BadWhitespace
  | InvalidEscape Char
  | InvalidHexChar Char
  | UnexpectedEOF

instance Show LexerError where
  show BadWhitespace = "Bad whitespace"
  show (InvalidEscape c) = "\"\\" <> [c] <> "\" is not a valid escape sequence"
  show (InvalidHexChar c) = show c <> " is not a valid hexadecimal character"
  show UnexpectedEOF = "Unexpected EOF"

instance ToError LexerError

data LexerState = LexerState
  { _lastIndent :: ByteString
  , _lastPoint :: Point
  , _path :: FilePath
  , _srcLines :: [(Word, ByteString, ByteString)]
  , _tokenBuffer :: [Token Span]
  } deriving Show

makeLenses ''LexerState

pointToLocation :: FilePath -> Point -> Location
pointToLocation path (P l c) = undefined

spanToLocation :: FilePath -> Span -> Location
spanToLocation path (S (P ls cs) (P le ce)) = undefined

stripComments :: ByteString -> ByteString
stripComments s = BS.take (findCommentStart 0 $ BS.toString s) s
  where findCommentStart n [] = n
        findCommentStart n (';':_) = n
        findCommentStart n (h:t) = findCommentStart (n+1) t

isWS :: Char -> Bool
isWS c = elem c [' ', '\n', '\r', '\t']

stripTrailingWS :: ByteString -> ByteString
stripTrailingWS s = BS.take (BS.length s - sRevWSLen) s
  where sRevWSLen = (BS.length . BS.fromString . takeWhile isWS . reverse . BS.toString) s

flatten2To3 :: (a, (b, c)) -> (a, b, c)
flatten2To3 (x, (y, z)) = (x, y, z)

mkLexerState :: FilePath -> ByteString -> LexerState
mkLexerState path s = LexerState
  { _lastIndent = ""
  , _lastPoint = P 0 0
  , _path = path
  , _srcLines = map flatten2To3 $
                filter (not . BS.null . snd . snd) $
                map (second (BS.span isWS . stripTrailingWS . stripComments)) $
                zip [1..] (BS.lines s)
  , _tokenBuffer = []
  }

computeLineTokens :: (MonadError Error m, MonadState LexerState m) => ByteString -> ByteString -> m [Token Span]
computeLineTokens ws line = do
  undefined

nextToken :: (MonadError Error m, MonadState LexerState m) => m (Token Span)
nextToken = use tokenBuffer >>= \case
  h:t -> do
    tokenBuffer .= t
    pure h
  [] -> use srcLines >>= \case
    [] -> (\p -> TokEOF $ S p p) <$> use lastPoint
    (lineNo, ws, line):t -> do
      srcLines .= t
      lastPoint .= P lineNo 0
      assign tokenBuffer =<< computeLineTokens ws line
      nextToken

nextToken' :: (MonadError Error m, MonadState LexerState m) => m (Token Location)
nextToken' = do
  tok <- nextToken
  path' <- use path
  pure (fmap (spanToLocation path') tok)

withLexerState :: (MonadError Error m, MonadReader (ReifiedLens' s LexerState) m, MonadState s m) =>
                  (forall m2. (MonadError Error m2, MonadState LexerState m2) => m2 a) -> m a
withLexerState m = do
  lsLens <- runLens <$> ask
  (e, ls') <- runIdentity . runStateT (runExceptT m) <$> use lsLens
  lsLens' <- runLens <$> ask
  lsLens' .= ls'
  liftEither e

throwLexerError :: (MonadError Error m, MonadState LexerState m) => LexerError -> m a
throwLexerError err = do
  path' <- use path
  loc <- pointToLocation path' <$> use lastPoint
  throwError $ mkChainedError err (Just loc) (CouldntParseFile path')

lexer :: (MonadError Error m, MonadState s m) => ReifiedLens' s LexerState -> (Token Location -> m a) -> m a
lexer lexerStateLens k = k =<< runReaderT (withLexerState nextToken') lexerStateLens
