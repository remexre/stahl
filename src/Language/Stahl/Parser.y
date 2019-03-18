{
module Language.Stahl.Parser where

import Language.Stahl.Ast
import Language.Stahl.Lexer
}

%lexer { lexer } { TokEOF }
%monad { P }
%name parser root
%tokentype { Token }
%token '_' { TokHole }

%%

root :: { () }
root : '_' { () }

{
type P a = [a]

happyError :: P a
{-
happyError = do
  toks <- get
  case toks of
    (Scanner.Error p:_) -> throwError (p, "Lexer error")
    (h:_) -> throwError (Scanner.posn h, "Unexpected token " <> show h)
-}
happyError = undefined

lexer :: (Token -> P a) -> P a
lexer = undefined

-- vim: set ft=happy :
}
