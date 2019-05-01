{-# LANGUAGE RankNTypes, StandaloneDeriving, UndecidableInstances #-}

module Language.Stahl.Ast
  ( Decl(..)
  , Expr(..)
  , GlobalName(..)
  , LocalName(..)
  , declAnnot
  , exprAnnot
  , exprCustom
  , transformDeclCustom
  , transformExprCustom
  , traverseDecl
  , traverseExpr
  , rewriteExpr
  , visitExpr
  ) where

import Control.Lens (Lens', Prism', lens, prism)
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import qualified Data.ByteString as BS (intercalate)
import qualified Data.ByteString.UTF8 as BS
import Data.Sequence (Seq(..))
import Language.Stahl.Internal.Ast.Builtins (Builtin(..))
import Language.Stahl.Internal.Value (Value(..))
import Language.Stahl.Internal.Util.Value (PP(..))

-- |A top-level declaration.
data Decl cE cD aE aD
  = CustomDecl (cD (Decl cE cD aE aD)) aD
  | Def LocalName (Expr cE aE) (Expr cE aE) aD
  | DefTy LocalName (Expr cE aE) (Seq (LocalName, Expr cE aE)) aD

deriving instance (Eq aD, Eq aE, Eq (cD (Decl cE cD aE aD)), Eq (Expr cE aE)) => Eq (Decl cE cD aE aD)
deriving instance (Show aD, Show aE, Show (cD (Decl cE cD aE aD)), Show (cE (Expr cE aE))) => Show (Decl cE cD aE aD)

declAnnot :: Lens' (Decl cE cD aE aD) aD
declAnnot = lens get set
  where get (CustomDecl _ a) = a
        get (Def _ _ _ a)    = a
        get (DefTy _ _ _ a)  = a
        set (CustomDecl c _) a = CustomDecl c a
        set (Def n t e _)    a = Def n t e a
        set (DefTy n k cs _) a = DefTy n k cs a

transformDeclCustom :: (Traversable cD, Traversable cE, Monad m)
                    => (cE (Expr cE' aE) -> aE -> m (Expr cE' aE))
                    -> (cD (Decl cE' cD' aE aD) -> aD -> m (Decl cE' cD' aE aD))
                    -> Decl cE cD aE aD -> m (Decl cE' cD' aE aD)
transformDeclCustom fE fD = traverseDecl helperE helperD pure pure
  where helperD c a = do
          c' <- traverse (transformDeclCustom fE fD) c
          a' <- a
          fD c' a'
        helperE c a = do
          c' <- traverse (transformExprCustom fE) c
          a' <- a
          fE c' a'

traverseDecl :: Applicative f
             => (cE (Expr cE aE) -> f aE' -> f (Expr cE' aE'))
             -> (cD (Decl cE cD aE aD) -> f aD' -> f (Decl cE' cD' aE' aD'))
             -> (aE -> f aE')
             -> (aD -> f aD')
             -> Decl cE cD aE aD -> f (Decl cE' cD' aE' aD')
traverseDecl fCE fCD fAE fAD (CustomDecl c a) = fCD c (fAD a)
traverseDecl fCE fCD fAE fAD (Def n t e a) =
  Def n <$> traverseExpr fCE fAE t <*> traverseExpr fCE fAE e <*> fAD a
traverseDecl fCE fCD fAE fAD (DefTy n k cs a) =
  let ctorHelper (name, ty) = (name,) <$> traverseExpr fCE fAE ty in
  DefTy n <$> traverseExpr fCE fAE k <*> traverse ctorHelper cs <*> fAD a

-- |The name of a global binding.
newtype GlobalName = GlobalName (ByteString, Seq ByteString, ByteString)
  deriving (Eq, Ord)

instance Show GlobalName where
  show (GlobalName (l, Empty, n)) = BS.toString (l <> ":" <> n)
  show (GlobalName (l, ms, n)) = BS.toString (l <> ":" <> ms' <> ":" <> n)
    where ms' = BS.intercalate ":" (toList ms)

instance PP GlobalName where
  pp = Symbol Nothing . BS.fromString . show

-- |The name of a local binding.
newtype LocalName = LocalName ByteString
  deriving (Eq, Ord)

instance Show LocalName where
  show (LocalName n) = BS.toString n

instance PP LocalName where
  pp (LocalName n) = Symbol Nothing n

-- |An expression.
data Expr c a
  = CustomExpr (c (Expr c a)) a
  | App (Expr c a) (Expr c a) a
  | Atom GlobalName a
  | Builtin Builtin a
  | Handle GlobalName (Expr c a) (Expr c a) a
  | Lam LocalName (Expr c a) a
  | Perform GlobalName (Expr c a) a
  | Pi (Maybe LocalName) (Expr c a) (Expr c a) (Seq GlobalName) a
  | Var LocalName a

deriving instance (Eq a, Eq (c (Expr c a))) => Eq (Expr c a)
deriving instance (Show a, Show (c (Expr c a))) => Show (Expr c a)

instance PP (c (Expr c a)) => PP (Expr c a) where
  pp (CustomExpr c _) = pp c
  pp (App l r _) = helper l (Cons Nothing (pp r) (Nil Nothing))
    where helper (App l' r' _) t = helper l' (Cons Nothing (pp r') t)
          helper e             t = Cons Nothing (pp e) t
  pp (Atom n _) = Symbol Nothing (BS.fromString ("#" <> show n <> "#"))
  pp (Builtin b _) = Symbol Nothing (BS.fromString ("#" <> show b <> "#"))
  pp (Handle eff e1 e2 _) = pp [Symbol Nothing "handle", pp eff, pp e1, pp e2]
  pp (Lam n e _) = helper [n] e
    where helper ns (Lam n' e' _) = helper (n':ns) e'
          helper ns e             = pp [Symbol Nothing "fn", pp (pp <$> reverse ns), pp e]
  pp (Perform eff b _) = pp [Symbol Nothing "perform", pp eff, pp b]
  pp (Pi (Just n) t1 t2 Empty _) = pp [Symbol Nothing "pi", pp n, pp t1, pp t2]
  pp (Pi Nothing t1 t2 Empty _)  = helper [t1] t2
    where helper t1s (Pi Nothing t1' t2' Empty _) = helper (t1':t1s) t2'
          helper t1s e                            = pp [Symbol Nothing "fun", pp (pp <$> reverse t1s), pp e]
  pp (Pi (Just n) t1 t2 effs _)  = pp [Symbol Nothing "pi", pp n, pp t1, pp t2, pp effs]
  pp (Pi Nothing t1 t2 effs _)   = pp [Symbol Nothing "pi", Symbol Nothing "_", pp t1, pp t2, pp effs]
  pp (Var n _) = pp n

-- A 'Lens' for the annotation part of the expression.
exprAnnot :: Lens' (Expr c a) a
exprAnnot = lens get set
  where get (CustomExpr _ a)     = a
        get (App _ _ a)          = a
        get (Atom _ a)           = a
        get (Builtin _ a)        = a
        get (Handle _ _ _ a)     = a
        get (Lam _ _ a)          = a
        get (Perform _ _ a)      = a
        get (Pi _ _ _ _ a)       = a
        get (Var _ a)            = a
        set (CustomExpr c _)     a = CustomExpr c a
        set (App e1 e2 _)        a = App e1 e2 a
        set (Atom n _)           a = Atom n a
        set (Builtin b _)        a = Builtin b a
        set (Handle eff e1 e2 _) a = Handle eff e1 e2 a
        set (Lam n b _)          a = Lam n b a
        set (Perform eff b _)    a = Perform eff b a
        set (Pi n t1 t2 effs _)  a = Pi n t1 t2 effs a
        set (Var n _)            a = Var n a

-- A 'Lens' for the annotation part of the expression.
exprCustom :: Prism' (Expr c a) (c (Expr c a), a)
exprCustom = prism from to
  where from (c, a) = CustomExpr c a
        to (CustomExpr c a) = Right (c, a)
        to expr = Left expr

rewriteExpr :: (Traversable c, Monad m) => (Expr c a -> m (Expr c a)) -> (Expr c a -> m (Expr c a))
rewriteExpr f = helper f <=< f
  where helper f (CustomExpr c a) = CustomExpr <$> traverse f c <*> pure a
        helper f (App e1 e2 a) =
          App <$> rewriteExpr f e1 <*> rewriteExpr f e2 <*> pure a
        helper f (Atom n a) = pure $ Atom n a
        helper f (Builtin b a) = pure $ Builtin b a
        helper f (Handle eff e1 e2 a) =
          Handle eff <$> rewriteExpr f e1 <*> rewriteExpr f e2 <*> pure a
        helper f (Lam n b a) = Lam n <$> rewriteExpr f b <*> pure a
        helper f (Perform eff b a) = Perform eff <$> rewriteExpr f b <*> pure a
        helper f (Pi n t1 t2 effs a) =
          Pi n <$> rewriteExpr f t1 <*> rewriteExpr f t2 <*> pure effs <*> pure a
        helper f (Var n a) = pure $ Var n a

transformExprCustom :: (Traversable c, Monad m)
                    => (c (Expr c' a) -> a -> m (Expr c' a))
                    -> Expr c a
                    -> m (Expr c' a)
transformExprCustom f = traverseExpr helper pure
  where helper c a = do
          c' <- traverse (transformExprCustom f) c
          a' <- a
          f c' a'

traverseExpr :: Applicative f
              => (c (Expr c a) -> f a' -> f (Expr c' a'))
              -> (a -> f a')
              -> Expr c a -> f (Expr c' a')
traverseExpr fC fA (CustomExpr c a) = fC c (fA a)
traverseExpr fC fA (App e1 e2 a) =
  App <$> traverseExpr fC fA e1 <*> traverseExpr fC fA e2 <*> fA a
traverseExpr fC fA (Atom n a) = Atom n <$> fA a
traverseExpr fC fA (Builtin b a) = Builtin b <$> fA a
traverseExpr fC fA (Handle eff e1 e2 a) =
  Handle eff <$> traverseExpr fC fA e1 <*> traverseExpr fC fA e2 <*> fA a
traverseExpr fC fA (Lam n b a) = Lam n <$> traverseExpr fC fA b <*> fA a
traverseExpr fC fA (Perform eff b a) = Perform eff <$> traverseExpr fC fA b <*> fA a
traverseExpr fC fA (Pi n t1 t2 effs a) =
  Pi n <$> traverseExpr fC fA t1 <*> traverseExpr fC fA t2 <*> pure effs <*> fA a
traverseExpr fC fA (Var n a) = Var n <$> fA a

-- |Post-order traversal over an expression.
visitExpr :: Applicative f => (Expr c a -> f ()) -> Expr c a -> f ()
visitExpr f e@(CustomExpr _ _) = f e
visitExpr f e@(App e1 e2 _) = visitExpr f e1 *> visitExpr f e2 *> f e
visitExpr f e@(Atom _ _) = f e
visitExpr f e@(Builtin _ _) = f e
visitExpr f e@(Handle _ e1 e2 _) = visitExpr f e1 *> visitExpr f e2 *> f e
visitExpr f e@(Lam _ b _) = visitExpr f b *> f e
visitExpr f e@(Perform _ b _) = visitExpr f b *> f e
visitExpr f e@(Pi _ t1 t2 _ _) = visitExpr f t1 *> visitExpr f t2 *> f e
visitExpr f e@(Var _ _) = f e
