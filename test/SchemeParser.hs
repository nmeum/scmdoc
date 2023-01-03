module SchemeParser where

import Test.Tasty
import Test.Tasty.HUnit

import Util
import SchemeDoc
import SchemeDoc.Parser.R7RS

schemeParser :: TestTree
schemeParser = testGroup "Tests for the Scheme parser"
    [ idParser, strParser, chrParser, boolParser, exprParser ]

idParser :: TestTree
idParser = testGroup "Identifier parser"
    [ testCase "Simple identifier" $ do
        assertEqual "" (Right $ [Id "foo"]) $ parse "foo"

    , testCase "Identifier with extented identifier character" $ do
        assertEqual "" (Right $ [Id "f%o!!"]) $ parse "f%o!!"

    , testCase "Identifier enclosed by vertical lines" $ do
        assertEqual "" (Right $ [Id "Hello"]) $ parse "|Hello|"

    , testCase "Parse the empty identifier" $ do
        assertEqual "" (Right $ [Id ""]) $ parse "||"

    , testCase "Identifier with inline hex escape" $ do
        assertEqual "" (Right $ [Id "Hello"]) $ parse "|H\\x65;llo|"

    , testCase "Identifier with invalid initial" $ do
        assertEqual "" "expecting digit, delimiter or end of input" (parseErrors scheme "(define 23foo x)")

    , testCase "Identifier with mnemonic escape" $ do
        assertEqual "" (Right $ [Id "b\ar"]) $ parse "|b\\ar|"
    ]

strParser :: TestTree
strParser = testGroup "String parser"
    [ testCase "Simple string" $ do
        assertEqual "" (Right $ [Str "foobar"]) $ parse "\"foobar\""

    , testCase "Escaped quote" $ do
        assertEqual "" (Right $ [Str "foo\"bar"]) $ parse "\"foo\\\"bar\""

    , testCase "Escaped newline" $ do
        assertEqual "" (Right $ [Str "foobar"]) $ parse "\"foo\\    \n   bar\""

    , testCase "String with inline hex escape" $ do
        assertEqual "" (Right $ [Str "Hello"]) $ parse "\"H\\x65;llo\""
    ]

chrParser :: TestTree
chrParser = testGroup "Character parser"
    [ testCase "Simple character" $ do
        assertEqual "" (Right $ [Char 'f']) $ parse "#\\f"

    , testCase "Character with character name" $ do
        assertEqual "" (Right $ [Char '\DEL']) $ parse "#\\delete"

    , testCase "Character with hex escape" $ do
        assertEqual "" (Right $ [Char 'a']) $ parse "#\\x61"
    ]

boolParser :: TestTree
boolParser = testGroup "Boolean parser"
    [ testCase "True" $ do
        assertEqual "Long form" (Right $ [Boolean True]) $ parse "#true"
        assertEqual "Short form" (Right $ [Boolean True]) $ parse "#t"

    , testCase "False" $ do
        assertEqual "Long form" (Right $ [Boolean False]) $ parse "#false"
        assertEqual "Short form" (Right $ [Boolean False]) $ parse "#f"
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
            $ parse "(define x 1) (define y 2)"

        assertEqual
            "With comment"
            (Right $ [
                List [ Id "define", Id "x", Number 1 ],
                List [ Id "define", Id "y", Number 2 ]
            ])
            $ parse "(define x 1)#|foo|#(define y 2)"

    , testCase "Quotations" $ do
        assertEqual
            "Quote identifier"
            (Right $ [Id "foobar"])
            $ parse "'foobar"

        assertEqual
            "Quote empty list"
            (Right $ [List []])
            $ parse "'()"

        assertEqual
            "Quote empty list with whitespaces"
            (Right $ [List []])
            $ parse "'  ()"

    , testCase "Miscellaneous" $ do
        assertEqual
            "Comment with leading spaces"
            (Right $ [])
            $ parse "  ;; foo\n"

        assertEqual
            "Commant with trailing spaces"
            (Right $ [List [Id "define", Id "x", Number 42]])
            $ parse "(define x\n\t;; foo\n\t42)"

        assertEqual
            "Documentation comment"
            (Right $ [DocComment "my comment", List [Id "define",Id "x",Number 2]])
            $ parse ";;> my comment\n(define x 2)"

        assertEqual
            "Whitespaces at start"
            (Right $ [Number 42])
            $ parse "  42"

        assertEqual
            "Whitespaces at end"
            (Right $ [Number 42])
            $ parse "42 "
    ]
