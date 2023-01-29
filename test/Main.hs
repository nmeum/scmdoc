import Test.Tasty

import Parser
import Library
import Formatter

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [
    schemeParser
    , libraryParser
    , formatter
  ]
