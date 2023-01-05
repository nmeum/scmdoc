module SchemeDoc.SyntaxError where

import SchemeDoc.Types

-- A syntax error occuring during processing of S-Expressions.
data SyntaxError = SyntaxError Sexp String
    deriving (Show, Eq)

-- Helper function for creating a new syntax error.
makeErr :: Sexp -> String -> Either SyntaxError a
makeErr expr msg = Left $ SyntaxError expr msg
