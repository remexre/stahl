{-# LANGUAGE RankNTypes, StandaloneDeriving, UndecidableInstances #-}

module Language.Stahl.Ast.Generic
  ( Decl(..)
  , Expr(..)
  , GlobalName(..)
  , LocalName(..)
  , declAnnot
  , exprAnnot
  , mapCustomDecl
  , traverseCustomDecl
  , traverseCustomDecl'
  , mapCustomExpr
  , traverseCustomExpr
  , traverseCustomExpr'
  ) where

import Control.Lens (Lens', lens)
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity(..))
import Data.Sequence (Seq)

data Decl cE cD aE aD
  = CustomDecl (cD (Decl cE cD aE aD)) aD
  | Def LocalName (Expr cE aE) (Expr cE aE) aD
  | DefTy LocalName (Expr cE aE) (Seq (LocalName, Expr cE aE)) aD
  deriving (Foldable, Functor, Traversable)

deriving instance (Show aD, Show (cD (Decl cE cD aE aD)), Show (Expr cE aE)) => Show (Decl cE cD aE aD)

declAnnot :: Lens' (Decl cE cD aE aD) aD
declAnnot = lens get set
  where get (CustomDecl _ a) = a
        get (Def _ _ _ a)    = a
        get (DefTy _ _ _ a)  = a
        set (CustomDecl c _) a = CustomDecl c a
        set (Def n t e _)    a = Def n t e a
        set (DefTy n k cs _) a = DefTy n k cs a

mapCustomDecl :: (Traversable cD, Traversable cD', Traversable cE, Traversable cE')
              => (cE (Expr cE' aE') -> cE' (Expr cE' aE'))
              -> (cD (Decl cE' cD' aE' aD') -> cD' (Decl cE' cD' aE' aD'))
              -> (aE -> aE')
              -> (aD -> aD')
              -> Decl cE cD aE aD -> Decl cE' cD' aE' aD'
mapCustomDecl fCE fCD fAE fAD = runIdentity . traverseCustomDecl fCE' fCD' fAE' fAD'
  where fCE' = Identity . fCE
        fCD' = Identity . fCD
        fAE' = Identity . fAE
        fAD' = Identity . fAD

traverseCustomDecl :: (Traversable cD, Traversable cD', Traversable cE, Traversable cE', Monad m)
                   => (cE (Expr cE' aE') -> m (cE' (Expr cE' aE')))
                   -> (cD (Decl cE' cD' aE' aD') -> m (cD' (Decl cE' cD' aE' aD')))
                   -> (aE -> m aE')
                   -> (aD -> m aD')
                   -> Decl cE cD aE aD -> m (Decl cE' cD' aE' aD')
traverseCustomDecl fCE fCD fAE fAD = traverseCustomDecl' fCE' fCD' fAE fAD
  where fCD' = fCD <=< traverse (traverseCustomDecl fCE fCD fAE fAD)
        fCE' = fCE <=< traverse (traverseCustomExpr fCE fAE)

traverseCustomDecl' :: (Traversable cD, Traversable cD', Traversable cE, Traversable cE', Monad m)
                    => (cE (Expr cE aE) -> m (cE' (Expr cE' aE')))
                    -> (cD (Decl cE cD aE aD) -> m (cD' (Decl cE' cD' aE' aD')))
                    -> (aE -> m aE')
                    -> (aD -> m aD')
                    -> Decl cE cD aE aD -> m (Decl cE' cD' aE' aD')
traverseCustomDecl' fCE fCD fAE fAD (CustomDecl c a) =
  CustomDecl <$> fCD c <*> fAD a
traverseCustomDecl' fCE fCD fAE fAD (Def n t e a) =
  Def n <$> traverseCustomExpr' fCE fAE t <*> traverseCustomExpr' fCE fAE e <*> fAD a
traverseCustomDecl' fCE fCD fAE fAD (DefTy n k cs a) =
    DefTy n <$> traverseCustomExpr' fCE fAE k <*> mapM ctorHelper cs <*> fAD a
  where ctorHelper (name, ty) = (name,) <$> traverseCustomExpr' fCE fAE ty

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
  deriving (Foldable, Functor, Traversable)

deriving instance (Show a, Show (c (Expr c a))) => Show (Expr c a)

exprAnnot :: Lens' (Expr c a) a
exprAnnot = lens get set
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

mapCustomExpr :: (Traversable c, Traversable c')
              => (c (Expr c' a') -> c' (Expr c' a'))
              -> (a -> a')
              -> Expr c a -> Expr c' a'
mapCustomExpr fC fA = runIdentity . traverseCustomExpr (Identity . fC) (Identity . fA)

traverseCustomExpr :: (Traversable c, Traversable c', Monad m)
               => (c (Expr c' a') -> m (c' (Expr c' a')))
               -> (a -> m a')
               -> Expr c a -> m (Expr c' a')
traverseCustomExpr fC fA = traverseCustomExpr' (fC <=< traverse (traverseCustomExpr fC fA)) fA

traverseCustomExpr' :: Monad m
                    => ((c (Expr c a)) -> m (c' (Expr c' a')))
                    -> (a -> m a')
                    -> Expr c a -> m (Expr c' a')
traverseCustomExpr' fC fA (CustomExpr c a) = CustomExpr <$> fC c <*> fA a
traverseCustomExpr' fC fA (App e1 e2 a) =
  App <$> traverseCustomExpr' fC fA e1 <*> traverseCustomExpr' fC fA e2 <*> fA a
traverseCustomExpr' fC fA (Atom n a) = Atom n <$> fA a
traverseCustomExpr' fC fA (Effect t1 t2 a) =
  Effect <$> traverseCustomExpr' fC fA t1 <*> traverseCustomExpr' fC fA t2 <*> fA a
traverseCustomExpr' fC fA (Handle eff e1 e2 a) =
  Handle eff <$> traverseCustomExpr' fC fA e1 <*> traverseCustomExpr' fC fA e2 <*> fA a
traverseCustomExpr' fC fA (Lam n e a) = Lam n <$> traverseCustomExpr' fC fA e <*> fA a
traverseCustomExpr' fC fA (Perform eff e a) = Perform eff <$> traverseCustomExpr' fC fA e <*> fA a
traverseCustomExpr' fC fA (Pi n t1 t2 effs a) =
  Pi n <$> traverseCustomExpr' fC fA t1 <*> traverseCustomExpr' fC fA t2 <*> pure effs <*> fA a
traverseCustomExpr' fC fA (Var n a) = Var n <$> fA a
