{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.UTF8 (ByteString, fromString)
import Data.Either (fromRight)
import Language.Stahl
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
  [ testGroup "Parser"
    [ ignoreTest $ testGroup "Properties"
      [ expectFail $
        testProperty "parse . show == id" $
        \value -> Just value == (parseOne . fromString $ show value)
      ]
    , testGroup "Unit Tests"
      [ testGroup "Parsing"
        [ ignoreTest $ goldenVsString "Syntax Guide Examples"
          "test-cases/parser.stahl.golden"
          (stringToLBS . unifyShowWith (unlines . map show) <$> (runExceptT $ parseFile "test-cases/parser.stahl"))
        , testCase "Nil" $
          assertEqual "" (Just $ Nil defaultLoc) (parseOne "()")
        , testCase "Nil via Group" $
          assertEqual "" (Just $ Nil defaultLoc) (parseOne "group")
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

parseOne :: ByteString -> Maybe Value
parseOne = helper . parse "<test:tests>"
  where helper (Right [x]) = Just x
        helper _ = Nothing

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
                    , String loc . fromString <$> arbitrary
                    , Symbol loc . fromString <$> arbitrary
                    , pure (Nil loc)
                    ]
          loc = Point "<unit tests>" 0 0
