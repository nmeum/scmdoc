module Library where

import Test.Tasty
import Test.Tasty.HUnit

import Util
import SchemeDoc
import SchemeDoc.Types
import SchemeDoc.Format.Library

libraryParser :: TestTree
libraryParser = testGroup "Tests for the Library parser"
    [ testCase "Library with exports and without declarations" $ do
        let input = ";;> my comment\n(define-library (foo) (export string-length))"
        let (Right expr) = parse input

        let (Right libraries) = findDocLibs expr
        assertEqual "Amount of libraries" 1 $ length libraries

        let library = snd $ head libraries
        assertEqual "Library name" "foo" $ libName library
        assertEqual "Exports string-length" True $ libExports library "string-length"
        assertEqual "Doesn't export foo" False $ libExports library "foo"

    , testCase "Multiple library definitions in a single file" $ do
        let input = ";;> foo library\n(define-library (foo))\n;;> bar library\n(define-library (bar))"
        let (Right expr) = parse input

        let (Right libs) = findDocLibs expr
        assertEqual "Amount of libraries" 2 $ length libs

        assertEqual "First library name" "foo" $ libName (snd $ libs !! 0)
        assertEqual "Second library name" "bar" $ libName (snd $ libs !! 1)

    , testCase "Multipart library name" $ do
        let input = ";;> my comment\n(define-library (foo  42 bar   23 baz))"
        let (Right expr) = parse input

        let (Right libs) = findDocLibs expr
        assertEqual "" "foo 42 bar 23 baz" $ libName (snd $ head libs)

    , testCase "Library with library declaration" $ do
        let input = ";;> scheme example library\n(define-library (scheme example) (begin (define x 23) (define y 42)))"
        let (Right expr) = parse input

        let (Right libs) = findDocLibs expr
        expanded <- libExpand (snd $ head libs)

        assertEqual
            ""
            [List
                [Id "begin",
                    List [Id "define",Id "x",Number 23],
                    List [Id "define",Id "y",Number 42]
                ]
            ] expanded

    , testCase "Library with multiple declarations" $ do
        let input = ";;> my lib\n(define-library (scheme example) (begin (define x 23)) (begin (define y 42)))"
        let (Right expr) = parse input

        let (Right libs) = findDocLibs expr
        expanded <- libExpand (snd $ head libs)

        assertEqual
            ""
            [List [Id "begin", List [Id "define",Id "y",Number 42]],
             List [Id "begin",List [Id "define",Id "x",Number 23]]]
            expanded

    , testCase "Library with include" $ do
        let input = ";;> doc comment\n(define-library (some example) (include \"test/testdata/simple-include.scm\"))"
        let (Right expr) = parse input

        let (Right libs) = findDocLibs expr
        expanded <- libExpand (snd $ head libs)

        assertEqual "" [List [Id "begin",List [Id "define",Id "x",Str "foo"]]] expanded
    ]
