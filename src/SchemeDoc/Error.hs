module SchemeDoc.Error where

import Control.Exception
import qualified Text.ParserCombinators.Parsec as P

import SchemeDoc.Types

-- A syntax error occuring during processing of S-Expressions.
data SyntaxError = SyntaxError Sexp String
    deriving (Show, Eq)

-- Helper function for creating a new syntax error.
makeErr :: Sexp -> String -> Either SyntaxError a
makeErr expr msg = Left $ SyntaxError expr msg

------------------------------------------------------------------------

data ExpandException = ErrSyntax SyntaxError | ErrParser P.ParseError
    deriving Show

instance Exception ExpandException

-- Helper function to throw a syntax error.
throwSyntax :: Sexp -> String -> IO a
throwSyntax e m = throwIO $ ErrSyntax (SyntaxError e m)
