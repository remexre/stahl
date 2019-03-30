{
module Language.Stahl.Parser
  ( parse
  , parseFile
  ) where

import Control.Lens (Lens', lens, (^.), (.=), use)
import Control.Monad.Error.Class (MonadError(..), liftEither)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.State.Strict (StateT(..), evalStateT)
import Data.ByteString (ByteString, readFile)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity(..))
import Data.Sequence (Seq, (|>), empty)
import Language.Stahl.Error (Error, ErrorKind(..), ToError(..))
import Language.Stahl.Lexer (LexerState, Token(..), getTokenData, lexOne, mkLexerState)
import Language.Stahl.Util (Location(Span), endPoint, file, startPoint)
import Language.Stahl.Util.LensedState (LensedStateT(..), liftLensedStateT)
import Language.Stahl.Value (Value(..), location)
import Prelude hiding (readFile)
}

%lexer { lexer } { TokEOF _ }
%monad { M }
%name parser root
%errorhandlertype explist
%error { happyError }
%tokentype { Token Location }
%token Dedent { TokDedent     $$ }
       Group  { TokGroup      $$ }
       Indent { TokIndent     $$ }
       NL     { TokNewline    $$ }
       '('    { TokParenOpen  $$ }
       ')'    { TokParenClose $$ }
       '|'    { TokPipe       $$ }
       Int    { TokInt        $$ }
       String { TokString     $$ }
       Symbol { TokSymbol     $$ }

%%

root :: { Seq Value }
root : NLs iexprs { $2 }

NLs :: { () }
NLs : { () }
    | NLs NL { () }

NLs1 :: { () }
NLs1 : NLs NL { () }

iexprs :: { Seq Value }
iexprs : { empty }
       | iexprs iexpr { $1 |> $2 }

iexprs1 :: { Seq Value }
iexprs1 : iexprs iexpr { $1 |> $2 }

iexpr :: { Value }
iexpr : head {% seqToConsList' $1 }
      | head body {% append `fmap` seqToConsList $1 <*> seqToConsList $2 }

head :: { Seq Value }
head : Group sexprs NLs1 { $2 }
     | sexprs1 NLs1 { $1 }

body :: { Seq Value }
body : Indent iexprs1 Dedent NLs { $2 }

sexprs :: { Seq Value }
sexprs : { empty }
       | sexprs sexpr { $1 |> $2 }

sexprs1 :: { Seq Value }
sexprs1 : sexprs sexpr { $1 |> $2 }

sexpr :: { Value }
sexpr : '(' NLs sexprListBody { $3 }
      | Int    { uncurry (flip Int) $1 }
      | String { uncurry (flip String) $1 }
      | Symbol { uncurry (flip Symbol) $1 }

sexprListBody :: { Value }
sexprListBody : sexpr NLs sexprListBody {% cons $1 $3 }
              | '|' NLs sexpr NLs ')' { $3 }
              | ')' { Nil $1 }

{
type M a = StateT ParserState (Either Error) a

data ParserState = ParserState
  { _lastToken :: Token Location
  , _lexerState :: LexerState
  } deriving Show

lastToken :: Lens' ParserState (Token Location)
lastToken = lens _lastToken (\p l -> p { _lastToken = l })

lexerState :: Lens' ParserState LexerState
lexerState = lens _lexerState (\p l -> p { _lexerState = l })

happyError :: (Token Location, [String]) -> M a
happyError (tok, exp) = throwError (mkChainedError msg (Just loc) $ CouldntParseFile $ loc^.file)
  where loc = getTokenData tok
        msg = "Got " <> show tok <> ", wanted one of " <> show exp

-- |Parses a string, returning the corresponding 'Value's.
parse :: MonadError Error m => FilePath -> ByteString -> m [Value]
parse path src = toList <$> parse' path src

-- |Parses a string, returning the corresponding 'Value's in a 'Seq'.
parse' :: MonadError Error m => FilePath -> ByteString -> m (Seq Value)
parse' path src = liftEither (evalStateT parser parserState)
  where parserState = ParserState
                        { _lastToken = error "No last token"
                        , _lexerState = mkLexerState path src
                        }

-- |Parses a file, returning the corresponding 'Value's.
parseFile :: (MonadError Error m, MonadIO m) => FilePath -> m [Value]
parseFile = fmap toList . parseFile'

-- |Parses a file, returning the corresponding 'Value's in a 'Seq'.
parseFile' :: (MonadError Error m, MonadIO m) => FilePath -> m (Seq Value)
parseFile' path = parse' path =<< liftIO (readFile path)

append :: Value -> Value -> Value
append (Cons l h t) t' = Cons l h (append t t')
append _ t' = t'

cons :: Value -> Value -> M Value
cons h t = do
  end <- lastPoint
  let spanBetween start end = Span f ls cs le ce
        where (f, ls, cs) = start^.startPoint
              (_, le, ce) = end^.endPoint
  pure $ Cons (spanBetween (h^.location) end) h t

seqToConsList :: Seq Value -> M Value
seqToConsList seq = do
  end <- lastPoint
  let spanBetween start end = Span f ls cs le ce
        where (f, ls, cs) = start^.startPoint
              (_, le, ce) = end^.endPoint
  let helper h t = Cons (spanBetween (h^.location) end) h t
  -- TODO: What's the monadic version of foldr?
  pure $ foldr helper (Nil end) seq

seqToConsList' :: Seq Value -> M Value
seqToConsList' seq =
  if length seq == 1 then
    pure (head (toList seq))
  else
    seqToConsList seq

lastPoint :: M Location
lastPoint = getTokenData <$> use lastToken

lexer :: (Token Location -> M a) -> M a
lexer k = do
  tok <- liftLensedStateT lexerState lexOne
  lastToken .= tok
  k tok

-- vim: set ft=happy :
}
