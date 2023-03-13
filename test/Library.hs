{-# LANGUAGE OverloadedStrings #-}

module Library where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text ()
import SchemeDoc
import qualified SchemeDoc.Format.Library as L
import SchemeDoc.Types
import Util

getLibs :: String -> [L.Library]
getLibs input = map snd libraries
  where
    -- Scheme S-expressions for the parsed input.
    (Right expr) = parse input

    -- Documented library definitions in the input
    (Right libraries) = findDocLibs expr

getLib :: String -> L.Library
getLib = head . getLibs

------------------------------------------------------------------------

libraryParser :: TestTree
libraryParser =
    testGroup
        "Tests for the Library parser"
        [ testCase "Library with exports and without declarations" $ do
            let lib = getLib ";;> my comment\n(define-library (foo) (export string-length))"

            assertEqual "Library name" "foo" $ L.name lib
            assertEqual "Exports string-length" True $ L.exports lib "string-length"
            assertEqual "Doesn't export foo" False $ L.exports lib "foo"
        , testCase "Library with renamed identifier" $ do
            let lib = getLib ";;> my comment\n(define-library (foo) (export (rename string-length strlen)))"

            assertEqual "Exports renamed internal" True $ L.exports lib "string-length"
            assertEqual "Doesn't export renamed external" False $ L.exports lib "strlen"
            assertEqual "Renamed identifier" (Just "strlen") $ L.externalId lib "string-length"
            assertEqual "Unknown identifier" Nothing $ L.externalId lib "foo"
        , testCase "Multiple lib definitions in a single file" $ do
            let libs = getLibs ";;> foo lib\n(define-library (foo))\n;;> bar library\n(define-library (bar))"
            assertEqual "Amount of libraries" 2 $ length libs

            assertEqual "First lib name" "foo" $ L.name (libs !! 0)
            assertEqual "Second lib name" "bar" $ L.name (libs !! 1)
        , testCase "Multipart lib name" $ do
            let lib = getLib ";;> my comment\n(define-library (foo  42 bar   23 baz))"
            assertEqual "" "foo 42 bar 23 baz" $ L.name lib
        , testCase "Library with lib declaration" $ do
            let lib = getLib ";;> scheme example lib\n(define-library (scheme example) (begin (define x 23) (define y 42)))"
            expanded <- L.expand lib

            assertEqual
                ""
                [ List
                    [ Id "begin"
                    , List [Id "define", Id "x", Number 23]
                    , List [Id "define", Id "y", Number 42]
                    ]
                ]
                expanded
        , testCase "Library with multiple declarations" $ do
            let lib = getLib ";;> my lib\n(define-library (scheme example) (begin (define x 23)) (begin (define y 42)))"
            expanded <- L.expand lib

            assertEqual
                ""
                [ List [Id "begin", List [Id "define", Id "y", Number 42]]
                , List [Id "begin", List [Id "define", Id "x", Number 23]]
                ]
                expanded
        , testCase "Library with include" $ do
            let lib = getLib ";;> doc comment\n(define-library (some example) (include \"test/testdata/simple-include.scm\"))"
            expanded <- L.expand lib

            assertEqual "" [List [Id "begin", List [Id "define", Id "x", Str "foo"]]] expanded
        ]
