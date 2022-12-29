import Test.Tasty

import SchemeParser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [
    schemeParser
  ]
