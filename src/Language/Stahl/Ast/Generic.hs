{-# LANGUAGE RankNTypes #-}

module Language.Stahl.Ast.Generic
  ( Annot(..)
  , Decl(..)
  , Expr(..)
  , GlobalName(..)
  , LocalName(..)
  , mapCustomDecl
  , mapCustomExpr
  ) where

import Control.Lens (Lens', lens)
import Data.ByteString (ByteString)
import Data.Sequence (Seq)

class Annot f where
  annot :: Lens' (f a) a

data Decl cE cD aE aD
  = CustomDecl (cD (Decl cE cD aE aD)) aD
  | Def LocalName (Expr cE aE) (Expr cE aE) aD
  deriving Functor

instance Annot (Decl cE cD aE) where
  annot = lens get set
    where get (CustomDecl _ a) = a
          get (Def _ _ _ a)    = a
          set (CustomDecl c _) a = CustomDecl c a
          set (Def n t e _)    a = Def n t e a

mapCustomDecl :: (cE (Expr cE aE) -> cE' (Expr cE' aE)) -> (cD (Decl cE cD aE aD) -> cD' (Decl cE' cD' aE aD)) -> Decl cE cD aE aD -> Decl cE' cD' aE aD
mapCustomDecl fE fD (CustomDecl c a) = CustomDecl (fD c) a
mapCustomDecl fE fD (Def n t e a) = Def n (mapCustomExpr fE t) (mapCustomExpr fE e) a

mapCustomDecl' :: (Functor cD, Functor cD', Functor cE, Functor cE')
               => (cE (Expr cE' aE) -> cE' (Expr cE' aE))
               -> (cD (Decl cE' cD' aE aD) -> cD' (Decl cE' cD' aE aD))
               -> Decl cE cD aE aD -> Decl cE' cD' aE aD
mapCustomDecl' fE fD = mapCustomDecl (fE . fmap (mapCustomExpr' fE)) (fD . fmap (mapCustomDecl' fE fD))

newtype GlobalName = GlobalName (ByteString, Seq ByteString, ByteString)
  deriving (Eq, Ord, Show)

newtype LocalName = LocalName ByteString
  deriving (Eq, Ord, Show)

data Expr c a
  = CustomExpr (c (Expr c a)) a
  | App (Expr c a) (Expr c a) a
  | Atom GlobalName a
  | Effect (Expr c a) (Expr c a) a
  | Handle GlobalName (Expr c a) (Expr c a) a
  | Lam LocalName (Expr c a) a
  | Perform GlobalName (Expr c a) a
  | Pi (Maybe LocalName) (Expr c a) (Expr c a) (Seq GlobalName) a
  | Var LocalName a
  deriving Functor

instance Annot (Expr c) where
  annot = lens get set
    where get (CustomExpr c a)     = a
          get (App e1 e2 a)        = a
          get (Atom n a)           = a
          get (Effect t1 t2 a)     = a
          get (Handle eff e1 e2 a) = a
          get (Lam n e a)          = a
          get (Perform eff e a)    = a
          get (Pi n t1 t2 effs a)  = a
          get (Var n a)            = a
          set (CustomExpr c _)     a = CustomExpr c a
          set (App e1 e2 _)        a = App e1 e2 a
          set (Atom n _)           a = Atom n a
          set (Effect t1 t2 _)     a = Effect t1 t2 a
          set (Handle eff e1 e2 _) a = Handle eff e1 e2 a
          set (Lam n e _)          a = Lam n e a
          set (Perform eff e _)    a = Perform eff e a
          set (Pi n t1 t2 effs _)  a = Pi n t1 t2 effs a
          set (Var n _)            a = Var n a

mapCustomExpr :: ((c (Expr c a)) -> (c' (Expr c' a))) -> Expr c a -> Expr c' a
mapCustomExpr f (CustomExpr c a) = CustomExpr (f c) a
mapCustomExpr f (App e1 e2 a) = App (mapCustomExpr f e1) (mapCustomExpr f e2) a
mapCustomExpr f (Atom n a) = Atom n a
mapCustomExpr f (Effect t1 t2 a) = Effect (mapCustomExpr f t1) (mapCustomExpr f t2) a
mapCustomExpr f (Handle eff e1 e2 a) = Handle eff (mapCustomExpr f e1) (mapCustomExpr f e2) a
mapCustomExpr f (Lam n e a) = Lam n (mapCustomExpr f e) a
mapCustomExpr f (Perform eff e a) = Perform eff (mapCustomExpr f e) a
mapCustomExpr f (Pi n t1 t2 effs a) = Pi n (mapCustomExpr f t1) (mapCustomExpr f t2) effs a
mapCustomExpr f (Var n a) = Var n a

mapCustomExpr' :: (Functor c, Functor c') => (c (Expr c' a) -> c' (Expr c' a)) -> Expr c a -> Expr c' a
mapCustomExpr' f = mapCustomExpr (f . fmap (mapCustomExpr' f))
