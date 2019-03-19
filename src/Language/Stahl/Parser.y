{
module Language.Stahl.Parser where

import Language.Stahl.Lexer
import Language.Stahl.Value
}

%lexer { lexer } { TokEOF }
%monad { P }
%name parser root
%tokentype { Token }
%token Dedent { TokDedent }
       Group  { TokGroup }
       Indent { TokIndent }
       NL  { TokNewline }
       '(' { TokParenOpen }
       ')' { TokParenClose }
       '|' { TokPipe }
       Int    { TokInt $$ }
       String { TokString $$ }
       Symbol { TokSymbol $$ }

%%

root :: { [()] }
root : NLs iexprs { $2 }

NLs :: { () }
NLs : { () }
    | NLs NL { () }

iexprs :: { [()] }
iexprs : { [] }
       | iexprs1 { $1 }

iexprs1 :: { [()] }
iexprs1 : iexpr { [$1] }

iexpr :: { () }
iexpr : head { () }
      | head body { () }

head :: { () }
head : Group sexprs NLs { () }
     | iexprs1 NLs { () }

body :: { () }
body : Indent iexprs1 Dedent NLs { () }

sexprs :: { [()] }
sexprs : { [] }
       | sexprs sexpr { $1 <> [$2] }

sexpr :: { () }
sexpr : '(' NLs sexprListBody { $3 }
      | Int    { () }
      | String { () }
      | Symbol { () }

sexprListBody :: { () }
sexprListBody : sexpr NLs sexprListBody { () }
              | '|' NLs sexpr NLs ')' { () }
              | ')' { () }

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

-- vim: set ft=happy :
}
