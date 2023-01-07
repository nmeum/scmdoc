import Test.Tasty

import Parser
import Library

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [
    schemeParser
    , libraryParser
  ]
