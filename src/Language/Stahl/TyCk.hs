module Language.Stahl.TyCk
  ( tyck
  ) where

import Language.Stahl.Ast.Generic (Expr(..))

-- |The typechecker for a generic 'Expr'.
tyck :: ()
tyck = undefined

tyckExpr :: Monad m => Expr c a -> m (Expr c a)
tyckExpr = undefined
