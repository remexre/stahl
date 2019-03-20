{-# LANGUAGE ConstraintKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable, RankNTypes #-}

module Language.Stahl.Lexer
  ( LexerError(..)
  , LexerState
  , Token(..)
  , lexer
  , mkLexerState
  ) where

import Control.Lens (ReifiedLens(..), ReifiedLens', (^.), (.=), use)
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (ExceptT(..), MonadError(..))
import Control.Monad.Loops (whileM_)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.State.Strict (MonadState(..), StateT(..))
import Data.ByteString.UTF8 (ByteString)
import Data.Int (Int64)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word)
import Language.Stahl.Error (Error, ErrorKind(..), Location(..), ToError(..))
import Prelude hiding (last)

type M s m = (MonadError Error m, MonadReader (ReifiedLens' s LexerState) m, MonadState s m)
data Point = P Word Word deriving (Eq, Show)
data Span = S Point Point deriving (Eq, Show)

data Token a
  = TokError
  | TokEOF a
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
  deriving (Foldable, Functor, Show, Traversable)

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
  { _chars :: [(Char, Point)]
  , _last :: Point
  , _lexerError :: LexerError -- May be _|_
  , _parenDepth :: Word
  , _path :: FilePath
  , _queued :: Seq (Token Span)
  , _queuedNL :: Maybe Point
  , _ws :: ByteString
  , _wsLevels :: [(Word, Bool)]
  } deriving Show

makeLenses ''LexerState

mkLexerState :: FilePath -> ByteString -> LexerState
mkLexerState path bs = LexerState
  { _chars = addPositionsToChars bs
  , _last = P 1 1
  , _lexerError = error "Tried to get lexer error where there was none!"
  , _parenDepth = 0
  , _path = path
  , _queued = Seq.empty
  , _queuedNL = Nothing
  , _ws = ""
  , _wsLevels = []
  }

addPositionsToChars :: ByteString -> [(Char, Point)]
addPositionsToChars bs = undefined

pointToLocation :: FilePath -> Point -> Location
pointToLocation path (P l c) = Point path l c

spanToLocation :: FilePath -> Span -> Location
spanToLocation path (S (P ls cs) (P le ce)) = Span path ls cs le ce

advanceChar :: M s m => m ()
advanceChar = do
  ls <- getLexerState
  lexerState <- getLexerStateLens
  case ls^.chars of
    [] -> throwLexerError UnexpectedEOF
    ((_, p):t) -> do
      lexerState.chars .= t
      lexerState.last .= p

peek :: M s m => m (Char, Point)
peek = do
  ls <- getLexerState
  case ls^.chars of
    [] -> throwLexerError UnexpectedEOF
    (h:_) -> pure h

nextChar :: M s m => m (Char, Point)
nextChar = peek <* advanceChar

eatWhitespace :: M s m => m ()
eatWhitespace = whileM_ (isWhitespace <$> peek) advanceChar
  where isWhitespace = undefined

lexHexDigit :: M s m => m Int64
lexHexDigit = do
  (c, _) <- nextChar
  case c of
    '0' -> pure 0
    '1' -> pure 1
    '2' -> pure 2
    '3' -> pure 3
    '4' -> pure 4
    '5' -> pure 5
    '6' -> pure 6
    '7' -> pure 7
    '8' -> pure 8
    '9' -> pure 9
    'A' -> pure 10
    'a' -> pure 10
    'B' -> pure 11
    'b' -> pure 11
    'C' -> pure 12
    'c' -> pure 12
    'D' -> pure 13
    'd' -> pure 13
    'E' -> pure 14
    'e' -> pure 14
    'F' -> pure 15
    'f' -> pure 15
    _ -> throwLexerError (InvalidHexChar c)

lexStringEscape :: M s m => m Char
lexStringEscape = do
  (c, _) <- nextChar
  case c of
    '\\' -> pure '\\'
    '"' -> pure '"'
    'n' -> pure '\n'
    'r' -> pure '\r'
    't' -> pure '\t'
    -- 'x' -> undefined
    -- 'u' -> undefined
    -- 'U' -> undefined
    _ -> throwLexerError (InvalidEscape c)

lexString :: M s m => Point -> m (ByteString, Span)
lexString start = do
  undefined

lexSymbolish :: M s m => Point -> m (ByteString, Span)
lexSymbolish start = do
  undefined

nextToken :: M s m => m (Token Span)
nextToken = pure $ TokEOF (S (P 0 0) (P 0 0))
  where onNL :: M s m => m ()
        onNL = undefined

nextToken' :: M s m => m (Token Location)
nextToken' = do
  tok <- nextToken
  ls <- getLexerState
  pure (spanToLocation (ls^.path) <$> tok)

getLexerState :: M s m => m LexerState
getLexerState = use =<< getLexerStateLens

getLexerStateLens :: (Functor f, M s m) => m ((LexerState -> f LexerState) -> s -> f s)
getLexerStateLens = runLens <$> ask

throwLexerError :: M s m => LexerError -> m a
throwLexerError err = do
  ls <- getLexerState
  let loc = pointToLocation (ls^.path) (ls^.last)
  throwError $ mkChainedError err (Just loc) (CouldntParseFile (ls^.path))

lexer :: (MonadError Error m, MonadState s m) => ReifiedLens' s LexerState -> (Token Location -> m a) -> m a
lexer lexerStateLens k = k =<< runReaderT nextToken' lexerStateLens
