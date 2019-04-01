{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.UTF8 (ByteString, fromString)
import Data.Either (fromRight)
import qualified Data.Map as Map
import Language.Stahl
import Language.Stahl.Modules (loadLibMeta)
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
  [ testGroup "Parser"
    [ testGroup "Properties"
      [ testProperty "parse . show == id" $
        \value -> Just value == (parseOne . fromString $ show value)
      ]
    , testGroup "Unit Tests"
      [ testGroup "Parsing"
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
      ]
    ]
  , expectFail $ testGroup "Integration"
    [ testCase "std can be imported" $
      assertFailure "TODO"
    ]
  ]

defaultLoc :: Location
defaultLoc = Span "<test:tests>" 0 0 0 0

must :: Show e => Either [e] a -> IO a
must (Left err) = assertFailure ("Error: " <> unlines (map show err))
must (Right x) = pure x

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
