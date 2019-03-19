{-# LANGUAGE FlexibleContexts, OverloadedStrings, TemplateHaskell #-}

module Language.Stahl.Lexer
  ( LexerError(..)
  , LexerState(..)
  , Token(..)
  , lexStahl
  , lexer
  ) where

import Control.Lens.TH (makeLenses)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..))
import Data.ByteString.UTF8 (ByteString)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Int (Int64)
import Data.Word
  ( Word
  , Word8
  , Word64
  )
import Language.Stahl.Error (ToError)

data Point = P Word Word deriving (Eq, Show)

data Span = S Point Point deriving (Eq, Show)

data Token
  = TokEOF
  | TokDedent
  | TokGroup
  | TokIndent
  | TokInt Int64
  | TokNewline
  | TokParenClose
  | TokParenOpen
  | TokPipe
  | TokString ByteString
  | TokSymbol ByteString
  deriving Show

data LexerError
  = BadWhitespace

instance Show LexerError where
  show BadWhitespace = "Bad whitespace"

instance ToError LexerError

data LexerState = LexerState
  { _chars :: [(Word8, Point)]
  , _last :: Point
  , _parenDepth :: Word
  , _queued :: Seq (Point, Span)
  , _queuedNL :: Maybe Point
  , _ws :: ByteString
  , _wsLevels :: [(Word, Bool)]
  } deriving Show

makeLenses ''LexerState

defaultLexerState :: ByteString -> LexerState
defaultLexerState bs = LexerState
  { _chars = addPositionsToChars bs
  , _last = P 1 1
  , _parenDepth = 0
  , _queued = Seq.empty
  , _queuedNL = Nothing
  , _ws = ""
  , _wsLevels = []
  }

addPositionsToChars :: ByteString -> [(Word8, Point)]
addPositionsToChars bs = undefined

eatWhitespace :: MonadState LexerState m => m ()
eatWhitespace = undefined

lexHexDigit :: MonadState LexerState m => m Word64
lexHexDigit = undefined

lexString :: (MonadError LexerError m, MonadState LexerState m) => Point -> m ByteString
lexString start = undefined

lexSymbolish :: (MonadError LexerError m, MonadState LexerState m) => Point -> m ByteString
lexSymbolish start = undefined

lexStringEscape :: (MonadError LexerError m, MonadState LexerState m) => m Char
lexStringEscape = undefined

lexStahl :: ByteString -> Either LexerError [Token]
lexStahl = undefined

lexer :: Monad m => (Token -> m a) -> m a
lexer k = k TokEOF -- TODO
