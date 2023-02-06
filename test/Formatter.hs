{-# LANGUAGE OverloadedStrings #-}
module Formatter where

import Test.Tasty
import Test.Tasty.HUnit

import Util
import Data.Text ()
import SchemeDoc.Format.Variable
import SchemeDoc.Format.Procedure
import SchemeDoc.Types

makeFmt :: (Sexp -> Maybe a) -> String -> (Maybe a)
makeFmt func scm = case parse scm of
                     Left  _ -> Nothing
                     Right s -> func $ head s

------------------------------------------------------------------------

formatter :: TestTree
formatter = testGroup "Tests for Scheme formatters"
    [ variableFmt, procedureFmt ]

variableFmt :: TestTree
variableFmt = testGroup "Formatter for Variable definitions"
    [ testCase "Simple variable definition" $ do
        assertEqual "String variable" (Just $ Variable "foo" (Id "bar"))
            $ makeFmt mkVariable "(define foo bar)"

        assertEqual "Number variable" (Just $ Variable "x" (Number 42))
            $ makeFmt mkVariable "(define   x   42)"

    , testCase "Invalid variable definition" $ do
        assertEqual "" Nothing $ makeFmt mkVariable "(define (foo) bar)"
    ]

procedureFmt :: TestTree
procedureFmt = testGroup "Formatter for procedure definitions"
    [ testCase "Simple procedure definition" $ do
        assertEqual "" (Just $ Procedure "foo" ["bar", "baz"] [(List [(Id "list"), (Id "bar")])])
            $ makeFmt mkProcedure "(define (foo bar baz) (list bar))"

        assertEqual "" (Just $ Procedure "id" ["x"] [(Id "x")])
            $ makeFmt mkProcedure "(define (id x) x)"

    , testCase "Procedure definition with period" $ do
        assertEqual "Multiple parameters"
                    (Just $ Procedure "foo" ["bar", ".", "baz"] [(Id "baz")])
            $ makeFmt mkProcedure "(define (foo bar . baz) baz)"

        assertEqual "Single parameter"
                    (Just $ Procedure "parse-seq" [".", "o"] [(List [(Id "parse-seq-list"), (Id "o")])])
            $ makeFmt mkProcedure "(define (parse-seq . o) (parse-seq-list o))"
    ]
