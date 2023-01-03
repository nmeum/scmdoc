module SchemeDoc where

-- Algebraic data type representing Scheme S-Expressions.
data Sexp = Str        String  -- "foo"
          | Id         String  -- foo
          | Symbol     String  -- 'foo
          | Char       Char    -- #\f
          | Boolean    Bool    -- #t
          | List       [Sexp]  -- ["foo" "bar"]
          | Number     Integer
          | DocComment String
    deriving (Show, Eq)

------------------------------------------------------------------------

-- A syntax error occuring during processing of S-Expressions.
data SyntaxError = SyntaxError Sexp String
    deriving (Show, Eq)

-- Helper function for creating a new syntax error.
makeErr :: Sexp -> String -> Either SyntaxError a
makeErr expr msg = Left $ SyntaxError expr msg

-- Approach:
--
--  1. Find all libraries
--  2. Within the libraries, expand the incldues
--  3. Filter out all S-expressions which are preceded by a doc-comment
--
--  4. Parse the doc-comments
--  5. Transform parsed doc-comments (e.g. into markdown)
--  6. For each: Write a file in the generated output language
