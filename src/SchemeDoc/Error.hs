-- | Data types used for error handling in SchemeDoc.
module SchemeDoc.Error where

import Control.Exception
import qualified Text.ParserCombinators.Parsec as P

import SchemeDoc.Types

-- | A syntax error occuring during further processing of a S-expression
-- with a 'SchemeDoc.Format.Formatter'. Currently, only used by the
-- 'SchemeDoc.Format.Library' 'SchemeDoc.Format.Formatter'.
data SyntaxError = SyntaxError
                    Sexp   -- ^ The S-expression for which an errror occured.
                    String -- ^ A human-readable description of the error.
    deriving (Show, Eq)

-- | Helper function for creating a new syntax error for a
-- given S-expression with a given error message.
makeErr :: Sexp -> String -> Either SyntaxError a
makeErr expr msg = Left $ SyntaxError expr msg

------------------------------------------------------------------------

-- | Exception occuring during the processing of a R7RS Scheme input.
--
-- Can either be a 'ErrParser' (occuring during inital parsing of
-- the input) or a 'ErrSyntax' (occuring during further processing of
-- parsed S-expressions).
data ExpandException = ErrParser P.ParseError | ErrSyntax SyntaxError
    deriving Show

instance Exception ExpandException

-- | Helper function to throw a 'SyntaxError'.
throwSyntax :: Sexp -> String -> IO a
throwSyntax e m = throwIO $ ErrSyntax (SyntaxError e m)
