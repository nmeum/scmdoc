module SchemeParser where

import Test.Tasty
import Test.Tasty.HUnit

import Types
import Parser.Scheme
import Text.ParserCombinators.Parsec

parseErrors :: Parser a -> String -> String
parseErrors p input =
    case parse p "" input of
        Left err ->
            last $ lines $ show err
        Right _  -> ""

------------------------------------------------------------------------

schemeParser :: TestTree
schemeParser = testGroup "Tests for the Scheme parser"
    [ idParser, strParser ]

idParser :: TestTree
idParser = testGroup "Identifier parser"
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

    , testCase "Identifier with invalid initial" $ do
        assertEqual "" "expecting \"|\" or letter" (parseErrors parseId "23foo")

    , testCase "Identifier with mnemonic escape" $ do
        assertEqual "" (Right $ Id "b\ar") $ parse parseId "" "|b\\ar|"
    ]

strParser :: TestTree
strParser = testGroup "String parser"
    [ testCase "Simple string" $ do
        assertEqual "" (Right $ Str "foobar") $ parse parseString "" "\"foobar\""

    , testCase "Escaped quote" $ do
        assertEqual "" (Right $ Str "foo\"bar") $ parse parseString "" "\"foo\\\"bar\""

    , testCase "Escaped newline" $ do
        assertEqual "" (Right $ Str "foobar") $ parse parseString "" "\"foo\\    \n   bar\""
    ]
