{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings, UndecidableInstances #-}

module Main (main) where

import Control.Monad ((<=<), mapM, void)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader)
import Control.Monad.Writer (execWriter)
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Bifunctor (Bifunctor(..))
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as BS
import Data.Default (Default(..))
import Data.Either (fromRight)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Language.Stahl
import Language.Stahl.Ast (rewriteExpr)
import Language.Stahl.Modules (loadLibMeta)
import Language.Stahl.TyCk (UnifVar)
import qualified Language.Stahl.Internal.Ast.Holed as Holed
import Language.Stahl.Internal.Util.MonadNonfatal (NonfatalT, runNonfatal, runNonfatalT)
import Language.Stahl.Internal.Util.Value (PP(..))
import Language.Stahl.Util
import Test.QuickCheck.Arbitrary (Arbitrary(..), vector)
import Test.QuickCheck.Gen (oneof, sized)
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests = testGroup "Tests"
  [ testGroup "AST"
    [ testGroup "rewriteExpr"
      [ testGroup "((fn (x) x) (fun (TYPE) TYPE))"
        [ testCase "Non-Modifying Traversal" $ do
            let helper expr = tell (showPP expr <> "\n") *> pure expr
                w = execWriter (rewriteExpr helper exampleIdOfTyToTy)
                expected = [ "((fn (x) x) (fun (#TypeOfTypes#) #TypeOfTypes#))"
                           , "(fn (x) x)"
                           , "x"
                           , "(fun (#TypeOfTypes#) #TypeOfTypes#)"
                           , "#TypeOfTypes#"
                           , "#TypeOfTypes#"
                           ]
            assertEqual "" (unlines expected) w
        , testCase "Rewrite App to (fn (x) x)" $ do
            let helper expr = tell (showPP expr <> "\n") *> pure (helper' expr)
                helper' (App _ _ _) = exampleId
                helper' expr = expr
                w = execWriter (rewriteExpr helper exampleIdOfTyToTy)
                expected = [ "((fn (x) x) (fun (#TypeOfTypes#) #TypeOfTypes#))"
                           , "x"
                           ]
            assertEqual "" (unlines expected) w
        , testCase "Rewrite to TYPE" $ do
            let helper expr = tell (showPP expr <> "\n") *> pure exampleTy
                w = execWriter (rewriteExpr helper exampleIdOfTyToTy)
                expected = [ "((fn (x) x) (fun (#TypeOfTypes#) #TypeOfTypes#))"
                           ]
            assertEqual "" (unlines expected) w
        ]
      ]
    , testGroup "Pretty-Printing"
      [ testCase "(x y z)" $ do
          let expr :: Holed.Expr
              expr = App (App (Var (LocalName "x") Nothing) (Var (LocalName "y") Nothing) Nothing) (Var (LocalName "z") Nothing) Nothing
          assertEqual "" "(x y z)" (showPP expr)
      , testCase "(x (y z))" $ do
          let expr :: Holed.Expr
              expr = App (Var (LocalName "x") Nothing) (App (Var (LocalName "y") Nothing) (Var (LocalName "z") Nothing) Nothing) Nothing
          assertEqual "" "(x (y z))" (showPP expr)
      , testCase "(fn (x y) x)" $ do
          let expr :: Holed.Expr
              expr = Lam (LocalName "x") (Lam (LocalName "y") (Var (LocalName "x") Nothing) Nothing) Nothing
          assertEqual "" "(fn (x y) x)" (showPP expr)
      , testCase "(fn* (x y) x)" $ do
          let expr = CustomExpr (Holed.ImplicitLam (LocalName "x")
                       (CustomExpr (Holed.ImplicitLam (LocalName "y")
                       (Var (LocalName "x") Nothing)) Nothing)) Nothing
          assertEqual "" "(fn* (x y) x)" (showPP expr)
      , testCase "(fun (T U) V)" $ do
          let expr :: Holed.Expr
              expr = Pi Nothing (Var (LocalName "T") Nothing)
                       (Pi Nothing (Var (LocalName "U") Nothing)
                          (Var (LocalName "V") Nothing) Empty Nothing) Empty Nothing
          assertEqual "" "(fun (T U) V)" (showPP expr)
      , testCase "(fun* (T U) V)" $ do
          let expr = CustomExpr (Holed.ImplicitPi Nothing (Var (LocalName "T") Nothing)
                       (CustomExpr (Holed.ImplicitPi Nothing (Var (LocalName "U") Nothing)
                         (Var (LocalName "V") Nothing) Empty) Nothing) Empty) Nothing
          assertEqual "" "(fun* (T U) V)" (showPP expr)
      ]
    , testGroup "Properties"
      [ testProperty "Holed.exprFromValue . pp == id" $
        \expr -> Right expr === (first show . runNonfatal . Holed.exprFromValue . pp $ expr)
      ]
    ]
  , testGroup "Parser"
    [ testGroup "Properties"
      [ testProperty "parse . show == id" $
        \value -> Just value === (parseOne . BS.fromString $ show value)
      ]
    , testGroup "Unit Tests"
      [ goldenVsString "Strings"
        "test-cases/parser/strings.stahl"
        (showParseFile "test-cases/parser/strings.stahl")
      , goldenVsString "Syntax Guide Examples"
        "test-cases/parser/doc-syntax-md.stahl.golden"
        (showParseFile "test-cases/parser/doc-syntax-md.stahl")
      , testCase "Nil" $
        assertEqual "" (Just $ Nil Nothing) (parseOne "()")
      , testCase "Nil via Group" $
        assertEqual "" (Just $ Nil Nothing) (parseOne "group")
      , testCase "1" $
        assertEqual "" (Just $ Int Nothing 1) (parseOne "1")
      , testCase "+1" $
        assertEqual "" (Just $ Int Nothing 1) (parseOne "+1")
      , testCase "1+" $
        assertEqual "" (Just $ Symbol Nothing "1+") (parseOne "1+")
      , testCase "-1" $
        assertEqual "" (Just $ Int Nothing (-1)) (parseOne "-1")
      , testCase "1-" $
        assertEqual "" (Just $ Symbol Nothing "1-") (parseOne "1-")
      , testCase "0" $
        assertEqual "" (Just $ Int Nothing 0) (parseOne "0")
      , testCase "-0" $
        assertEqual "" (Just $ Int Nothing 0) (parseOne "-0")
      , testCase "--0" $
        assertEqual "" (Just $ Symbol Nothing "--0") (parseOne "--0")
      ]
    ]
  , testGroup "Typechecker"
    [ testCase "((fn (x) x) (fun (TYPE) TYPE)) : TYPE" $ do
        res <- must . flip runReader def . runNonfatalT $ tyck exampleIdOfTyToTy Nothing
        assertEqual "" (exampleIdOfTyToTy, exampleTy) res
    ]
  , testGroup "Modules"
    [ testCase "Loads std's lib.stahld" $ do
        meta <- must =<< (runNonfatalT $ loadLibMeta "std/lib.stahld")
        let expected = LibMeta
              { _libName = LibName
                { _name = "std"
                , _major = 0
                , _minor = 0
                , _patch = 0
                }
              , _deps = Map.fromList
                [ ( "compiler-builtins"
                  , LibName
                    { _name = "compiler-builtins"
                    , _major = 0
                    , _minor = 0
                    , _patch = 0
                    }
                  )
                ]
              }
        assertEqual "" expected meta
    ]
  , testGroup "Utils"
    [ testGroup "takeWhileBS"
      [ testCase "all" $
        assertEqual "" (takeWhileBS (const True) "foobar") "foobar"
      , testCase "none" $
        assertEqual "" (takeWhileBS (const False) "foobar") ""
      , testCase "some" $
        assertEqual "" (takeWhileBS (/= 'b') "foobar") "foo"
      ]
    ]
  , testGroup "Integration Tests"
    [ testCase "std can be loaded" $
      must' =<< (runNonfatalT $ loadLibrary "std")
    ]
  ]

exampleId :: Expr (Const UnifVar) (Maybe Location)
exampleId = Lam (LocalName "x") (Var (LocalName "x") Nothing) Nothing

exampleIdOfTyToTy :: Expr (Const UnifVar) (Maybe Location)
exampleIdOfTyToTy = App exampleId pi Nothing
  where pi = Pi Nothing exampleTy exampleTy Seq.empty Nothing

exampleTy :: Expr (Const UnifVar) (Maybe Location)
exampleTy = Builtin TypeOfTypes Nothing

must :: Show e => Either [e] a -> IO a
must (Left err) = assertFailure ("Error: " <> unlines (map show err))
must (Right x) = pure x

must' :: Show e => Either [e] a -> IO ()
must' = void . must

must1 :: Show e => Either e a -> IO a
must1 (Left err) = assertFailure ("Error: " <> show err)
must1 (Right x) = pure x

must1' :: Show e => NonfatalT e IO a -> IO a
must1' = must <=< runNonfatalT

parseOne :: ByteString -> Maybe Value
parseOne = helper . parse "<test:tests>"
  where helper (Right [x]) = Just x
        helper _ = Nothing

showParseFile :: FilePath -> IO LBS.ByteString
showParseFile path = stringToLBS . unifyShowWith (unlines . map show) <$> (runExceptT $ parseFile path)

stringToLBS :: String -> LBS.ByteString
stringToLBS = LBS.fromStrict . BS.fromString

unifyShowWith :: (Show e) => (a -> String) -> Either e a -> String
unifyShowWith _ (Left e) = show e
unifyShowWith f (Right x) = f x

arbitraryHelper :: [Gen a] -> [Gen a -> Gen a] -> Gen a
arbitraryHelper baseClauses indClauses = sized helper
  where helper 0 = oneof baseClauses
        helper n = oneof ((($ helper (n-1)) <$> indClauses) <> baseClauses)

arbitraryName :: Gen ByteString
arbitraryName = do
  len <- choose (1, 8)
  BS.fromString <$> (vectorOf len $ oneof $ [choose ('A', 'Z'), choose ('a', 'z')])

arbitraryString :: Gen ByteString
arbitraryString = do
  len <- choose (0, 8)
  BS.fromString <$> vectorOf len (choose (' ', '~'))

instance Arbitrary Builtin where
  arbitrary = oneof (pure <$> [TypeOfTypes, TypeOfTypeOfTypes])

instance Arbitrary GlobalName where
  arbitrary = do
    libName <- arbitraryName
    modLen <- choose (1, 3)
    modName <- Seq.fromList <$> vectorOf modLen arbitraryName
    valName <- arbitraryName
    pure $ GlobalName (libName, modName, valName)

instance Arbitrary LocalName where
  arbitrary = LocalName <$> arbitraryName

instance (Arbitrary (e (Expr e a)), Default a) => Arbitrary (Expr e a) where
  arbitrary = arbitraryHelper
    [ CustomExpr <$> arbitrary <*> pure def
    -- , Atom <$> arbitrary <*> pure def
    -- , Builtin <$> arbitrary <*> pure def
    , Var <$> arbitrary <*> pure def
    ]
    [ -- \r -> App (Expr c a) (Expr c a) a
    -- , \r -> Handle GlobalName (Expr c a) (Expr c a) a
    -- , \r -> Lam LocalName (Expr c a) a
    -- , \r -> Perform GlobalName (Expr c a) a
    -- , \r -> Pi (Maybe LocalName) (Expr c a) (Expr c a) (Seq GlobalName) a
    ]

instance Default a => Arbitrary (Holed.ExprCustom (Expr Holed.ExprCustom a)) where
  arbitrary = arbitraryHelper
    [ Holed.Hole <$> arbitraryName
    , Holed.ImplicitLam <$> arbitrary <*> arbitrary
    , Holed.ImplicitPi <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]
    [
    ]

instance Arbitrary Value where
  arbitrary = arbitraryHelper
    [ Int Nothing <$> arbitrary
    , String Nothing <$> arbitraryString
    , Symbol Nothing <$> arbitraryName
    , pure (Nil Nothing)
    ]
    [ \r -> Cons Nothing <$> r <*> r
    ]
