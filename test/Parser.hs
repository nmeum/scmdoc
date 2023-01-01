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
    [ idParser, strParser, chrParser, boolParser, exprParser ]

idParser :: TestTree
idParser = testGroup "Identifier parser"
    [ testCase "Simple identifier" $ do
        assertEqual "" (Right $ [Id "foo"]) $ parse scheme "" "foo"

    , testCase "Identifier with extented identifier character" $ do
        assertEqual "" (Right $ [Id "f%o!!"]) $ parse scheme "" "f%o!!"

    , testCase "Identifier enclosed by vertical lines" $ do
        assertEqual "" (Right $ [Id "Hello"]) $ parse scheme "" "|Hello|"

    , testCase "Parse the empty identifier" $ do
        assertEqual "" (Right $ [Id ""]) $ parse scheme "" "||"

    , testCase "Identifier with inline hex escape" $ do
        assertEqual "" (Right $ [Id "Hello"]) $ parse scheme "" "|H\\x65;llo|"

    , testCase "Identifier with invalid initial" $ do
        assertEqual "" "expecting digit, delimiter or end of input" (parseErrors scheme "(define 23foo x)")

    , testCase "Identifier with mnemonic escape" $ do
        assertEqual "" (Right $ [Id "b\ar"]) $ parse scheme "" "|b\\ar|"
    ]

strParser :: TestTree
strParser = testGroup "String parser"
    [ testCase "Simple string" $ do
        assertEqual "" (Right $ [Str "foobar"]) $ parse scheme "" "\"foobar\""

    , testCase "Escaped quote" $ do
        assertEqual "" (Right $ [Str "foo\"bar"]) $ parse scheme "" "\"foo\\\"bar\""

    , testCase "Escaped newline" $ do
        assertEqual "" (Right $ [Str "foobar"]) $ parse scheme "" "\"foo\\    \n   bar\""

    , testCase "String with inline hex escape" $ do
        assertEqual "" (Right $ [Str "Hello"]) $ parse scheme "" "\"H\\x65;llo\""
    ]

chrParser :: TestTree
chrParser = testGroup "Character parser"
    [ testCase "Simple character" $ do
        assertEqual "" (Right $ [Char 'f']) $ parse scheme "" "#\\f"

    , testCase "Character with character name" $ do
        assertEqual "" (Right $ [Char '\DEL']) $ parse scheme "" "#\\delete"

    , testCase "Character with hex escape" $ do
        assertEqual "" (Right $ [Char 'a']) $ parse scheme "" "#\\x61"
    ]

boolParser :: TestTree
boolParser = testGroup "Boolean parser"
    [ testCase "True" $ do
        assertEqual "Long form" (Right $ [Boolean True]) $ parse scheme "" "#true"
        assertEqual "Short form" (Right $ [Boolean True]) $ parse scheme "" "#t"

    , testCase "False" $ do
        assertEqual "Long form" (Right $ [Boolean False]) $ parse scheme "" "#false"
        assertEqual "Short form" (Right $ [Boolean False]) $ parse scheme "" "#f"
    ]

------------------------------------------------------------------------

exprParser :: TestTree
exprParser = testGroup "Expression parser"
    [ testCase "Delimited expressions" $ do
        assertEqual
            "With whitespaces"
            (Right $ [
                List [ Id "define", Id "x", Number 1 ],
                List [ Id "define", Id "y", Number 2 ]
            ])
            $ parse scheme "" "(define x 1) (define y 2)"

        assertEqual
            "With comment"
            (Right $ [
                List [ Id "define", Id "x", Number 1 ],
                Comment "foo",
                List [ Id "define", Id "y", Number 2 ]
            ])
            $ parse scheme "" "(define x 1)#|foo|#(define y 2)"
    ]
