import Test.Tasty

import Formatter
import Library
import Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ schemeParser
        , libraryParser
        , formatter
        ]
