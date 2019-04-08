{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Main (main) where

import Control.Monad ((<=<), void)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader)
import Control.Monad.Writer (execWriter)
import Control.Monad.Writer.Class (MonadWriter(..))
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.UTF8 (ByteString, fromString)
import Data.Default (Default(..))
import Data.Either (fromRight)
import Data.Functor.Const (Const(..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Language.Stahl
import Language.Stahl.Ast (rewriteExpr)
import Language.Stahl.Modules (loadLibMeta)
import Language.Stahl.TyCk (UnifVar)
import Language.Stahl.Util
import Language.Stahl.Util.MonadNonfatal (runNonfatalT)
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
            let helper expr = tell (show expr <> "\n") *> pure expr
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
            let helper expr = tell (show expr <> "\n") *> pure (helper' expr)
                helper' (App _ _ _) = exampleId
                helper' expr = expr
                w = execWriter (rewriteExpr helper exampleIdOfTyToTy)
                expected = [ "((fn (x) x) (fun (#TypeOfTypes#) #TypeOfTypes#))"
                           , "x"
                           ]
            assertEqual "" (unlines expected) w
        , testCase "Rewrite to TYPE" $ do
            let helper expr = tell (show expr <> "\n") *> pure exampleTy
                w = execWriter (rewriteExpr helper exampleIdOfTyToTy)
                expected = [ "((fn (x) x) (fun (#TypeOfTypes#) #TypeOfTypes#))"
                           ]
            assertEqual "" (unlines expected) w
        ]
      ]
    ]
  , testGroup "Parser"
    [ testGroup "Properties"
      [ testProperty "parse . show == id" $
        \value -> Just value == (parseOne . fromString $ show value)
      ]
    , testGroup "Unit Tests"
      [ goldenVsString "Strings"
        "test-cases/parser/strings.stahl"
        (showParseFile "test-cases/parser/strings.stahl")
      , goldenVsString "Syntax Guide Examples"
        "test-cases/parser/doc-syntax-md.stahl.golden"
        (showParseFile "test-cases/parser/doc-syntax-md.stahl")
      , testCase "Nil" $
        assertEqual "" (Just $ Nil defaultLoc) (parseOne "()")
      , testCase "Nil via Group" $
        assertEqual "" (Just $ Nil defaultLoc) (parseOne "group")
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

defaultLoc :: Location
defaultLoc = Span "<test:tests>" 0 0 0 0

exampleId :: Expr (Const UnifVar) (Maybe Location)
exampleId = Lam (LocalName "x") (Var (LocalName "x") loc) loc
  where loc = Just defaultLoc

exampleIdOfTyToTy :: Expr (Const UnifVar) (Maybe Location)
exampleIdOfTyToTy = App exampleId pi (Just defaultLoc)
  where pi = Pi Nothing exampleTy exampleTy Seq.empty (Just defaultLoc)

exampleTy :: Expr (Const UnifVar) (Maybe Location)
exampleTy = Builtin TypeOfTypes (Just defaultLoc)

must :: Show e => Either [e] a -> IO a
must (Left err) = assertFailure ("Error: " <> unlines (map show err))
must (Right x) = pure x

must' :: Show e => Either [e] a -> IO ()
must' = void . must

parseOne :: ByteString -> Maybe Value
parseOne = helper . parse "<test:tests>"
  where helper (Right [x]) = Just x
        helper _ = Nothing

showParseFile :: FilePath -> IO LBS.ByteString
showParseFile path = stringToLBS . unifyShowWith (unlines . map show) <$> (runExceptT $ parseFile path)

stringToLBS :: String -> LBS.ByteString
stringToLBS = LBS.fromStrict . fromString

unifyShowWith :: (Show e) => (a -> String) -> Either e a -> String
unifyShowWith _ (Left e) = show e
unifyShowWith f (Right x) = f x

instance Arbitrary Value where
  arbitrary = sized arbitrary'
    where arbitrary' 0 = oneof clauses
          arbitrary' n = oneof ((Cons loc <$> arbitrary' (n-1) <*> arbitrary' (n-1)) : clauses)
          clauses = [ Int loc <$> arbitrary
                    , do
                        len <- choose (0, 32)
                        String loc . fromString <$> vectorOf len (choose (' ', '~'))
                    , do
                        len <- choose (1, 32)
                        let symbolish = oneof $ [choose ('A', 'Z'), choose ('a', 'z')]
                        Symbol loc . fromString <$> vectorOf len symbolish
                    , pure (Nil loc)
                    ]
          loc = Point "<test:tests-quickcheck>" 0 0
