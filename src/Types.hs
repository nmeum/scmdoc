module Types where

data Sexp = Str      String -- "foo"
          | Id       String -- foo
          | Symbol   String -- 'foo
          | List     [Sexp] -- ["foo" "bar"]
          | ConsList [Sexp] -- ("foo" . bar)
    deriving (Show, Eq)
