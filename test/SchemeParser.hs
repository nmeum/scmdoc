module SchemeParser where

import Test.Tasty
import Test.Tasty.HUnit

import Types
import Parser.Scheme
import Text.ParserCombinators.Parsec

schemeParser :: TestTree
schemeParser = testGroup "Tests for the Scheme parser"
    [ testCase "Simple identifier" $ do
        assertEqual "" (Right $ Id "foo") $ parse parseId "" "foo"

    , testCase "Identifier with extented identifier character" $ do
        assertEqual "" (Right $ Id "f%o!!") $ parse parseId "" "f%o!!"

    , testCase "Identifier enclosed by vertical lines" $ do
        assertEqual "" (Right $ Id "Hello") $ parse parseId "" "|Hello|"

    , testCase "Parse the empty identifier" $ do
        assertEqual "" (Right $ Id "") $ parse parseId "" "||"

    , testCase "Identifier with inline hex escape" $ do
        assertEqual "" (Right $ Id "Hello") $ parse parseId "" "|H\\x65;llo|"
    ]
