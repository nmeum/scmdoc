module Types where

data Sexp = Atom     String
          | Str      String
          | Symbol   String
          | List     [Sexp]
          | ConsList [Sexp]
    deriving (Show, Eq)
