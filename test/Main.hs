import Test.Tasty

import Formatter
import Library
import Parser
import Record

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ schemeParser
        , libraryParser
        , formatter
        , record
        ]
