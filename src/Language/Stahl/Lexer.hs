{-# LANGUAGE RankNTypes #-}

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
  , (%=)
  , (+=)
  , (-=)
  , (.=)
  , (^.)
  , lens
  , use
  )
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (MonadError(..))
import Control.Monad.State.Strict (MonadState(..))
import qualified Data.ByteString as BS (null)
import qualified Data.ByteString.UTF8 as BS
import Data.ByteString.UTF8 (ByteString)
import Data.Int (Int64)
import Data.Sequence (Seq(Empty, (:<|)), (|>))
import qualified Data.Sequence as Seq
import Language.Stahl.Error (Error, ErrorKind(..), ToError(..))
import Language.Stahl.Util (Location(..))
import Language.Stahl.Value (isSymbolish, symbolishAsNumber)

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
  , _indents :: Seq ByteString
  , _lastPoint :: Point
  , _parenDepth :: Int
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

isWSToken :: Token a -> Bool
isWSToken (TokDedent _) = True
isWSToken (TokIndent _) = True
isWSToken _ = False

stripTrailingWS :: ByteString -> ByteString
stripTrailingWS s = BS.take (BS.length s - sRevWSLen) s
  where sRevWSLen = (BS.length . BS.fromString . takeWhile isWS . reverse . BS.toString) s

flatten2To3 :: (a, (b, c)) -> (a, b, c)
flatten2To3 (x, (y, z)) = (x, y, z)

mkLexerState :: FilePath -> ByteString -> LexerState
mkLexerState path s = LexerState
  { _currentLine = ""
  , _indents = Empty
  , _lastPoint = P 0 0
  , _parenDepth = 0
  , _path = path
  , _srcLines = map flatten2To3 $
                filter (not . BS.null . snd . snd) $
                map (second (BS.span isWS {- . stripTrailingWS . stripComments -})) $
                zip [1..] (BS.lines s)
  , _tokenBuffer = []
  }

computeWSTokens :: (MonadError Error m, MonadState LexerState m) => ByteString -> m [Token Span]
computeWSTokens ws = do
  oldIndents <- use indents
  indents .= Empty
  computeWSTokens' ws oldIndents

computeWSTokens' :: (MonadError Error m, MonadState LexerState m) => ByteString -> Seq ByteString -> m [Token Span]
computeWSTokens' s Empty = do
  if BS.null s then
    pure []
  else do
    indents %= (|> s)
    P l e <- use lastPoint
    pure [TokIndent $ S (P l (e - (fromIntegral $ BS.length s))) (P l e)]
computeWSTokens' s (h :<| t) = do
  if BS.null s then do
    p <- use lastPoint
    pure (replicate (1 + Seq.length t) (TokDedent $ S p p))
  else do
    let l = BS.length h
    if h == BS.take l s then do
      indents %= (|> h)
      computeWSTokens' (BS.drop l s) t
    else
      undefined

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
  Just '(' -> do
    parenDepth += 1
    (:) <$> (TokParenOpen <$> advance') <*> lexLine
  Just ')' -> do
    parenDepth -= 1
    (:) <$> (TokParenClose <$> advance') <*> lexLine
  Just '|' -> (:) <$> (TokPipe <$> advance') <*> lexLine
  Just ';' -> pure []
  Just c | isSymbolish c -> (:) <$> lexSymbolish <*> lexLine
  Just c -> error ("TODO lexLine " <> show c)
  Nothing -> pure []

nextLine :: (MonadError Error m, MonadState LexerState m) => m (Token Span)
nextLine = do
  start <- use lastPoint
  let end = P (start^.line + 1) 0
  pure (TokNewline (S start end))

lexString :: (MonadError Error m, MonadState LexerState m) => [Char] -> m ByteString
lexString buf = peek >>= \case
  Just '"' -> advance >> pure (BS.fromString (reverse buf))
  Just '\\' -> advance >> peek >>= \case
    Just '\\' -> advance >> lexString ('\\':buf)
    Just '"' -> advance >> lexString ('"':buf)
    Just c -> throwLexerError (InvalidEscape c)
    Nothing -> throwLexerError UnexpectedNL
  Just c -> advance >> lexString (c:buf)
  Nothing -> throwLexerError UnexpectedNL

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
  else case symbolishAsNumber s of
    Just n -> TokInt (n, loc)
    Nothing -> TokSymbol (s, loc)

nextToken :: (MonadError Error m, MonadState LexerState m) => m (Token Span)
nextToken = use tokenBuffer >>= \case
  h:t -> do
    tokenBuffer .= t
    pure h
  [] -> use srcLines >>= \case
    [] -> do
      p <- use lastPoint
      let s = S p p
      Seq.length <$> use indents >>= \case
        0 -> pure (TokEOF s)
        n -> do
          tokenBuffer .= replicate n (TokDedent s)
          indents .= Empty
          nextToken
    (lineNo, ws, lineS):t -> do
      srcLines .= t
      lastPoint .= P lineNo (fromIntegral $ BS.length ws)
      currentLine .= lineS
      pd <- use parenDepth
      lineTokens <- lexLine
      if null lineTokens || pd > 0 then
        pure ()
        -- indents .= lastIndents
      else do
        wsTokens <- computeWSTokens ws
        nlToken <- nextLine
        tokenBuffer .= (wsTokens <> lineTokens <> [nlToken])
      nextToken

throwLexerError :: (MonadError Error m, MonadState LexerState m) => LexerError -> m a
throwLexerError err = do
  path' <- use path
  loc <- pointToLocation path' <$> use lastPoint
  throwError $ mkChainedError err (Just loc) (CouldntParseFile path')

lexOne :: (MonadError Error m, MonadState LexerState m) => m (Token Location)
lexOne = do
  tok <- nextToken
  path' <- use path
  pure (fmap (spanToLocation path') tok)
