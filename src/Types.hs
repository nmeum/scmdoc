module Types where

data Sexp = Str      String  -- "foo"
          | Id       String  -- foo
          | Symbol   String  -- 'foo
          | Char     Char    -- #\f
          | Boolean  Bool    -- #t
          | List     [Sexp]  -- ["foo" "bar"]
          | Number   Integer
    deriving (Show, Eq)
