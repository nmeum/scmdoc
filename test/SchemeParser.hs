module SchemeParser where

import Test.Tasty
import Test.Tasty.HUnit

import Types
import Parser.Scheme
import Text.ParserCombinators.Parsec

schemeParser :: TestTree
schemeParser = testGroup "Tests for the Scheme parser"
    [ testCase "Parse an identifier with a hex escape" $ do
        assertEqual "simple symbol" (Right $ Id "foo") $ parse parseId "" "foo"
    ]
