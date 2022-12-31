module SchemeParser where

import Test.Tasty
import Test.Tasty.HUnit

import Types
import Parser.Scheme
import Text.ParserCombinators.Parsec hiding (string)

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
        assertEqual "" (Right $ Id "foo") $ parse identifier "" "foo"

    , testCase "Identifier with extented identifier character" $ do
        assertEqual "" (Right $ Id "f%o!!") $ parse identifier "" "f%o!!"

    , testCase "Identifier enclosed by vertical lines" $ do
        assertEqual "" (Right $ Id "Hello") $ parse identifier "" "|Hello|"

    , testCase "Parse the empty identifier" $ do
        assertEqual "" (Right $ Id "") $ parse identifier "" "||"

    , testCase "Identifier with inline hex escape" $ do
        assertEqual "" (Right $ Id "Hello") $ parse identifier "" "|H\\x65;llo|"

    , testCase "Identifier with invalid initial" $ do
        assertEqual "" "expecting letter or \"|\"" (parseErrors identifier "23foo")

    , testCase "Identifier with mnemonic escape" $ do
        assertEqual "" (Right $ Id "b\ar") $ parse identifier "" "|b\\ar|"
    ]

strParser :: TestTree
strParser = testGroup "String parser"
    [ testCase "Simple string" $ do
        assertEqual "" (Right $ Str "foobar") $ parse string "" "\"foobar\""

    , testCase "Escaped quote" $ do
        assertEqual "" (Right $ Str "foo\"bar") $ parse string "" "\"foo\\\"bar\""

    , testCase "Escaped newline" $ do
        assertEqual "" (Right $ Str "foobar") $ parse string "" "\"foo\\    \n   bar\""

    , testCase "String with inline hex escape" $ do
        assertEqual "" (Right $ Str "Hello") $ parse string "" "\"H\\x65;llo\""
    ]
