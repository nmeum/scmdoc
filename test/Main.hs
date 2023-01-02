import Test.Tasty

import SchemeParser
import Library

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [
    schemeParser
    , libraryParser
  ]
