module Language.Stahl.Ast.Generic
  ( Annot(..)
  , Decl(..)
  , Expr(..)
  , GlobalName
  , LocalName(..)
  ) where

import Control.Lens
  ( Lens'
  , Prism
  , lens
  , prism
  )
import Data.Bifunctor (Bifunctor(..))
import Data.ByteString (ByteString)
import Data.Sequence (Seq)

class Annot f where
  annot :: Lens' (f a) a

data Decl cD cE a
  deriving Functor

type GlobalName = (ByteString, Seq ByteString, ByteString)

newtype LocalName = LocalName ByteString
  deriving (Eq, Ord, Show)

data Expr c a
  = Custom c a
  | App (Expr c a) (Expr c a) a
  | Atom GlobalName a
  | Effect (Expr c a) (Expr c a) a
  | Handle GlobalName (Expr c a) (Expr c a) a
  | Lam LocalName (Expr c a) a
  | Perform GlobalName (Expr c a) a
  | Pi (Maybe LocalName) (Expr c a) (Expr c a) (Seq GlobalName) a
  | Var LocalName a
  deriving Functor

instance Bifunctor Expr where
  bimap fC fA (Custom c a) = Custom (fC c) (fA a)
  bimap fC fA (App e1 e2 a) = App (bimap fC fA e1) (bimap fC fA e2) (fA a)
  bimap fC fA (Atom n a) = Atom n (fA a)
  bimap fC fA (Effect t1 t2 a) = Effect (bimap fC fA t1) (bimap fC fA t2) (fA a)
  bimap fC fA (Handle eff e1 e2 a) = Handle eff (bimap fC fA e1) (bimap fC fA e2) (fA a)
  bimap fC fA (Lam n e a) = Lam n (bimap fC fA e) (fA a)
  bimap fC fA (Perform eff e a) = Perform eff (bimap fC fA e) (fA a)
  bimap fC fA (Pi n t1 t2 effs a) = Pi n (bimap fC fA t1) (bimap fC fA t2) effs (fA a)
  bimap fC fA (Var n a) = Var n (fA a)

instance Annot (Expr c) where
  annot = lens get set
    where get (Custom c a)         = a
          get (App e1 e2 a)        = a
          get (Atom n a)           = a
          get (Effect t1 t2 a)     = a
          get (Handle eff e1 e2 a) = a
          get (Lam n e a)          = a
          get (Perform eff e a)    = a
          get (Pi n t1 t2 effs a)  = a
          get (Var n a)            = a
          set (Custom c _)         a = Custom c a
          set (App e1 e2 _)        a = App e1 e2 a
          set (Atom n _)           a = Atom n a
          set (Effect t1 t2 _)     a = Effect t1 t2 a
          set (Handle eff e1 e2 _) a = Handle eff e1 e2 a
          set (Lam n e _)          a = Lam n e a
          set (Perform eff e _)    a = Perform eff e a
          set (Pi n t1 t2 effs _)  a = Pi n t1 t2 effs a
          set (Var n _)            a = Var n a
