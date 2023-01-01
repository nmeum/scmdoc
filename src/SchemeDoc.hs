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
