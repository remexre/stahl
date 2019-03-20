module Language.Stahl.Lexer
  ( LexerError(..)
  , LexerState
  , Token(..)
  , lexStahl
  , lexer
  ) where

import Control.Lens (use)
import Control.Lens.TH (makeLenses)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Loops (whileM_)
import Control.Monad.State (MonadState(..), StateT(..))
import Data.ByteString.UTF8 (ByteString)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Int (Int64)
import Data.Word
  ( Word
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
  | InvalidHexChar Char
  | UnexpectedEOF

instance Show LexerError where
  show BadWhitespace = "Bad whitespace"
  show UnexpectedEOF = "Unexpected EOF"

instance ToError LexerError

data LexerState = LexerState
  { _chars :: [(Char, Point)]
  , _last :: Point
  , _parenDepth :: Word
  , _queued :: Seq (Token, Span)
  , _queuedNL :: Maybe Point
  , _ws :: ByteString
  , _wsLevels :: [(Word, Bool)]
  } deriving Show

makeLenses ''LexerState

mkLexerState :: ByteString -> LexerState
mkLexerState bs = LexerState
  { _chars = addPositionsToChars bs
  , _last = P 1 1
  , _parenDepth = 0
  , _queued = Seq.empty
  , _queuedNL = Nothing
  , _ws = ""
  , _wsLevels = []
  }

addPositionsToChars :: ByteString -> [(Char, Point)]
addPositionsToChars bs = undefined

eatWhitespace :: (MonadError LexerError m, MonadState LexerState m) => m ()
eatWhitespace = whileM_ (isWhitespace <$> peek) nextChar
  where isWhitespace = undefined

lexHexDigit :: (MonadError LexerError m, MonadState LexerState m) => m Word64
lexHexDigit = undefined

lexString :: (MonadError LexerError m, MonadState LexerState m) => Point -> m ByteString
lexString start = undefined

lexSymbolish :: (MonadError LexerError m, MonadState LexerState m) => Point -> m ByteString
lexSymbolish start = undefined

lexStringEscape :: (MonadError LexerError m, MonadState LexerState m) => m Char
lexStringEscape = undefined

nextChar :: (MonadError LexerError m, MonadState LexerState m) => m (Char, Point)
nextChar = do
  undefined

nextToken :: (MonadError LexerError m, MonadState LexerState m) => m (Token, Span)
nextToken = undefined

peek :: (MonadError LexerError m, MonadState LexerState m) => m Char
peek = use chars >>= \case
  [] -> throwError UnexpectedEOF
  ((ch, _):_) -> pure ch

lexStahl :: ByteString -> Either LexerError [Token]
lexStahl = undefined

lexer :: Monad m => (Token -> m a) -> m a
lexer k = k TokEOF -- TODO
