{-# LANGUAGE DeriveFunctor, RankNTypes #-}

module Language.Stahl.Lexer
  ( LexerError(..)
  , LexerState
  , Token(..)
  , getTokenData
  , lexOne
  , mkLexerState
  ) where

import Control.Arrow (second)
import Control.Lens
  ( Lens'
  , ReifiedLens(..)
  , ReifiedLens'
  , (%=)
  , (+=)
  , (.=)
  , (<>=)
  , (^.)
  , assign
  , lens
  , use
  )
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
import Data.Maybe (maybeToList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Debug.Trace (traceShowM)
import Language.Stahl.Error (Error, ErrorKind(..), ToError(..))
import Language.Stahl.Util (Location(..), takeWhileBS)

data Point = P !Word !Word deriving (Eq, Show)
data Span = S !Point !Point deriving (Eq, Show)

line :: Lens' Point Word
line = lens (\(P l _) -> l) (\(P _ c) l -> P l c)

column :: Lens' Point Word
column = lens (\(P _ c) -> c) (\(P l _) c -> P l c)

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

getTokenData :: Token a -> a
getTokenData (TokEOF a) = a
getTokenData (TokDedent a) = a
getTokenData (TokGroup a) = a
getTokenData (TokIndent a) = a
getTokenData (TokNewline a) = a
getTokenData (TokParenClose a) = a
getTokenData (TokParenOpen a) = a
getTokenData (TokPipe a) = a
getTokenData (TokInt (_, a)) = a
getTokenData (TokString (_, a)) = a
getTokenData (TokSymbol (_, a)) = a

data LexerError
  = BadWhitespace
  | InvalidEscape Char
  | InvalidHexChar Char
  | UnexpectedNL

instance Show LexerError where
  show BadWhitespace = "Bad whitespace"
  show (InvalidEscape c) = "\"\\" <> [c] <> "\" is not a valid escape sequence"
  show (InvalidHexChar c) = show c <> " is not a valid hexadecimal character"
  show UnexpectedNL = "Unexpected newline"

instance ToError LexerError

data LexerState = LexerState
  { _currentLine :: ByteString
  , _indents :: [ByteString]
  , _lastPoint :: Point
  , _path :: FilePath
  , _srcLines :: [(Word, ByteString, ByteString)]
  , _tokenBuffer :: [Token Span]
  } deriving Show

makeLenses ''LexerState

pointToLocation :: FilePath -> Point -> Location
pointToLocation file (P l c) = Point file l c

spanToLocation :: FilePath -> Span -> Location
spanToLocation file (S (P ls cs) (P le ce)) = Span file ls cs le ce

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
  { _currentLine = ""
  , _indents = []
  , _lastPoint = P 0 0
  , _path = path
  , _srcLines = map flatten2To3 $
                filter (not . BS.null . snd . snd) $
                map (second (BS.span isWS . stripTrailingWS . stripComments)) $
                zip [1..] (BS.lines s)
  , _tokenBuffer = []
  }

computeWSTokens :: (MonadError Error m, MonadState LexerState m) => ByteString -> m (Maybe (Token Span))
computeWSTokens ws = do
  lastPoint.column .= fromIntegral (BS.length ws)
  computeWSTokens' ws =<< use indents

computeWSTokens' :: (MonadError Error m, MonadState LexerState m) => ByteString -> [ByteString] -> m (Maybe (Token Span))
computeWSTokens' s [] = do
  if BS.null s then
    pure Nothing
  else do
    indents %= (s:)
    P l e <- use lastPoint
    pure $ Just $ TokIndent $ S (P l (e - (fromIntegral $ BS.length s))) (P l e)

advance :: (MonadError Error m, MonadState LexerState m) => m ()
advance = do
  s <- use currentLine
  case BS.uncons s of
    Just (_, t) -> do
      currentLine .= t
      lastPoint.column += 1
    Nothing -> throwLexerError UnexpectedNL

advance' :: (MonadError Error m, MonadState LexerState m) => m Span
advance' = do
  start <- use lastPoint
  advance
  end <- use lastPoint
  pure (S start end)

peek :: (MonadError Error m, MonadState LexerState m) => m (Maybe Char)
peek = fmap fst . BS.uncons <$> use currentLine

lexLine :: (MonadError Error m, MonadState LexerState m) => m [Token Span]
lexLine = do
 peek >>= \case
  Just '\t' -> advance >> lexLine
  Just ' ' -> advance >> lexLine
  Just '"' -> do
    start <- use lastPoint
    advance
    hStr <- lexString []
    end <- use lastPoint
    let h = TokString (hStr, S start end)
    t <- lexLine
    pure (h:t)
  Just '(' -> (:) <$> (TokParenOpen <$> advance') <*> lexLine
  Just ')' -> (:) <$> (TokParenClose <$> advance') <*> lexLine
  Just '|' -> (:) <$> (TokPipe <$> advance') <*> lexLine
  Just c | isSymbolish c -> (:) <$> lexSymbolish <*> lexLine
  Just c -> error ("TODO lexLine " <> show c)
  Nothing -> do
    start <- use lastPoint
    let end = P (start^.line + 1) 0
    pure [TokNewline (S start end)]

lexString :: (MonadError Error m, MonadState LexerState m) => [Char] -> m ByteString
lexString buf = peek >>= \case
  Just '"' -> advance >> pure (BS.fromString (reverse buf))
  Just c -> error ("TODO lexString " <> show c)
  Nothing -> throwLexerError UnexpectedNL

isSymbolish :: Char -> Bool
isSymbolish c = inRange c '0' '9' || inRange c 'A' 'Z' || inRange c 'a' 'z' || c `elem` punct
  where inRange n s e = fromEnum s <= fromEnum n && fromEnum n <= fromEnum e
        punct = [] -- TODO

lexSymbolish :: (MonadError Error m, MonadState LexerState m) => m (Token Span)
lexSymbolish = do
  start <- use lastPoint
  (s, rest) <- BS.span isSymbolish <$> use currentLine
  lastPoint.column += fromIntegral (BS.length s)
  currentLine .= rest
  end <- use lastPoint
  let loc = S start end
  pure $ if s == "group" then
    TokGroup loc
  else
    TokSymbol (s, loc)

nextToken :: (MonadError Error m, MonadState LexerState m) => m (Token Span)
nextToken = use tokenBuffer >>= \case
  h:t -> do
    tokenBuffer .= t
    pure h
  [] -> use srcLines >>= \case
    [] -> (\p -> TokEOF $ S p p) <$> use lastPoint
    (lineNo, ws, lineS):t -> do
      srcLines .= t
      lastPoint .= P lineNo 0
      wsTokens <- computeWSTokens ws
      currentLine .= lineS
      lineTokens <- lexLine
      tokenBuffer .= (maybeToList wsTokens <> lineTokens)
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

lexOne :: (MonadError Error m, MonadState s m) => Lens' s LexerState -> m (Token Location)
lexOne l = runReaderT (withLexerState nextToken') (Lens l)
