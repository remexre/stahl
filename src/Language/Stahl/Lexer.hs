{-# LANGUAGE FlexibleContexts, OverloadedStrings, TemplateHaskell #-}

module Language.Stahl.Lexer
  ( LexerError(..)
  , LexerState(..)
  , Token(..)
  , lex
  ) where

import Control.Lens.TH (makeLenses)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..))
import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
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
  | TokHole
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
  , _path :: ByteString
  , _queued :: Seq (Point, Span)
  , _queuedNL :: Maybe Point
  , _ws :: ByteString
  , _wsLevels :: [(Word, Bool)]
  } deriving Show

makeLenses ''LexerState

defaultLexerState :: ByteString -> ByteString -> LexerState
defaultLexerState bs path = LexerState
  { _chars = addPositionsToChars bs
  , _last = P 1 1
  , _parenDepth = 0
  , _path = path
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
