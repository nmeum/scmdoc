module SchemeDoc where

-- Algebraic data type representing Scheme S-Expressions.
data Sexp = Str      String  -- "foo"
          | Id       String  -- foo
          | Symbol   String  -- 'foo
          | Char     Char    -- #\f
          | Boolean  Bool    -- #t
          | List     [Sexp]  -- ["foo" "bar"]
          | Number   Integer
          | Comment  String
    deriving (Show, Eq)

------------------------------------------------------------------------

-- A syntax error occuring during processing of S-Expressions.
data SyntaxError = SyntaxError Sexp String
    deriving (Show, Eq)

-- Helper function for creating a new syntax error.
makeErr :: Sexp -> String -> Either SyntaxError a
makeErr expr msg = Left $ SyntaxError expr msg
