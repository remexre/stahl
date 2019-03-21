{
module Language.Stahl.Parser
  ( parse
  , parseFile
  ) where

import Control.Lens (Lens', ReifiedLens(..), lens)
import Control.Monad.Error.Class (MonadError(..), liftEither)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (StateT(..), evalStateT)
import Data.ByteString (ByteString, readFile)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity(..))
import Data.Sequence (Seq, (|>), empty)
import Language.Stahl.Error (Error, Location)
import Language.Stahl.Lexer (LexerState, Token(..), lexer, mkLexerState)
import Language.Stahl.Value (Value(..))
import Prelude hiding (readFile)
}

%lexer { lexer (Lens lexerState) } { TokEOF _ }
%monad { M }
%name parser root
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
iexpr : head { undefined }
      | head body { undefined }

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
      | Int    { undefined }
      | String { undefined }
      | Symbol { undefined }

sexprListBody :: { Value }
sexprListBody : sexpr NLs sexprListBody { undefined }
              | '|' NLs sexpr NLs ')' { undefined }
              | ')' { undefined }

{
type M a = StateT ParserState (Either Error) a

data ParserState = ParserState
  { _lexerState :: LexerState
  }

lexerState :: Lens' ParserState LexerState
lexerState = lens _lexerState (\p l -> p { _lexerState = l })

happyError :: M a
{-
happyError = do
  toks <- get
  case toks of
    (Scanner.Error p:_) -> throwError (p, "Lexer error")
    (h:_) -> throwError (Scanner.posn h, "Unexpected token " <> show h)
-}
happyError = undefined

-- |Parses a string, returning the corresponding 'Value's.
parse :: MonadError Error m => FilePath -> ByteString -> m [Value]
parse path src = toList <$> parse' path src

-- |Parses a string, returning the corresponding 'Value's in a 'Seq'.
parse' :: MonadError Error m => FilePath -> ByteString -> m (Seq Value)
parse' path src = liftEither (evalStateT parser parserState)
  where parserState = ParserState lexerState
        lexerState = mkLexerState path src

-- |Parses a file, returning the corresponding 'Value's.
parseFile :: (MonadError Error m, MonadIO m) => FilePath -> m [Value]
parseFile = fmap toList . parseFile'

-- |Parses a file, returning the corresponding 'Value's in a 'Seq'.
parseFile' :: (MonadError Error m, MonadIO m) => FilePath -> m (Seq Value)
parseFile' path = parse' path =<< liftIO (readFile path)

-- vim: set ft=happy :
}
