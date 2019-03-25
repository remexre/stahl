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
    [ testGroup "Properties"
      [ testProperty "parse . show == id" $
        \value -> Just value == (parseOne . fromString $ show value)
      ]
    , testGroup "Unit Tests"
      [ testGroup "Parsing"
        [ goldenVsString "Strings"
          "test-cases/parser/strings.stahl.golden"
          (showParseFile "test-cases/parser/strings.stahl")
        , goldenVsString "Syntax Guide Examples"
          "test-cases/parser/doc-syntax-md.stahl.golden"
          (showParseFile "test-cases/parser/doc-syntax-md.stahl")
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

showParseFile :: FilePath -> IO LBS.ByteString
showParseFile path = stringToLBS . unifyShowWith (unlines . map show) <$> (runExceptT $ parseFile path)

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
                    , do
                        len <- choose (0, 32)
                        String loc . fromString <$> vectorOf len (choose (' ', '~'))
                    , do
                        len <- choose (1, 32)
                        let symbolish = oneof $ map pure ['x']
                        Symbol loc . fromString <$> vectorOf len symbolish
                    , pure (Nil loc)
                    ]
          loc = Point "<test:tests-quickcheck>" 0 0
