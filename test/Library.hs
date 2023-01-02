module Library where

import Test.Tasty
import Test.Tasty.HUnit

import Util
import SchemeDoc
import SchemeDoc.Scheme.Library

libraryParser :: TestTree
libraryParser = testGroup "Tests for the Library parser"
    [ testCase "Library with exports and without declarations" $ do
        let input = "(define-library (foo) (export string-length))"
        let (Right expr) = parse input

        let (Right libraries) = findLibraries expr
        assertEqual "Amount of libraries" 1 $ length libraries

        let library = head libraries
        assertEqual "Library name" "foo" $ libName library
        assertEqual "Exports string-length" True $ libExports library "string-length"
        assertEqual "Doesn't export foo" False $ libExports library "foo"

    , testCase "Multiple library definitions in a single file" $ do
        let input = "(define-library (foo)) (define-library (bar))"
        let (Right expr) = parse input

        let (Right libs) = findLibraries expr
        assertEqual "Amount of libraries" 2 $ length libs

        assertEqual "First library name" "foo" $ libName (libs !! 0)
        assertEqual "Second library name" "bar" $ libName (libs !! 1)

    , testCase "Multipart library name" $ do
        let input = "(define-library (foo  42 bar   23 baz))"
        let (Right expr) = parse input

        let (Right libs) = findLibraries expr
        assertEqual "" "foo 42 bar 23 baz" $ libName (head libs)

    , testCase "Library with library declaration" $ do
        let input = "(define-library (scheme example) (begin (define x 23) (define y 42)))"
        let (Right expr) = parse input

        let (Right libs) = findLibraries expr
        expanded <- libExpand (head libs)

        assertEqual
            ""
            [List
                [Id "begin",
                    List [Id "define",Id "x",Number 23],
                    List [Id "define",Id "y",Number 42]
                ]
            ] expanded

    , testCase "Library with multiple declarations" $ do
        let input = "(define-library (scheme example) (begin (define x 23)) (begin (define y 42)))"
        let (Right expr) = parse input

        let (Right libs) = findLibraries expr
        expanded <- libExpand (head libs)

        assertEqual
            ""
            [List [Id "begin", List [Id "define",Id "y",Number 42]],
             List [Id "begin",List [Id "define",Id "x",Number 23]]]
            expanded

    , testCase "Library with include" $ do
        let input = "(define-library (some example) (include \"test/testdata/simple-include.scm\"))"
        let (Right expr) = parse input

        let (Right libs) = findLibraries expr
        expanded <- libExpand (head libs)

        assertEqual "" [List [Id "begin",List [Id "define",Id "x",Str "foo"]]] expanded
    ]
