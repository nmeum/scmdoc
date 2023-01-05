module SchemeDoc.Types where

import Data.List (intercalate)

-- A documented S-expression
type Documented = (String, Sexp)

-- Algebraic data type representing Scheme S-Expressions.
data Sexp = Str        String  -- "foo"
          | Id         String  -- foo
          | Symbol     String  -- 'foo
          | Char       Char    -- #\f
          | Boolean    Bool    -- #t
          | List       [Sexp]  -- ["foo" "bar"]
          | Number     Integer
          | DocComment String
    deriving (Eq)

instance Show Sexp where
    show (Str s) = "\"" ++ s ++ "\"" -- TODO: Perform escaping
    show (Id i) = i
    show (Symbol s) = "'" ++ s
    show (Char c) = "#\\" ++ [c]
    show (Boolean b) = if b then "#t" else "#f"
    show (List a) = "(" ++ (intercalate " " (map show a)) ++ ")"
    show (Number n) = show n
    show (DocComment c) = ";;> " ++ c ++ "\n"

-- Traverse a Scheme source, i.e. a list of S-expressions.
walk :: (b -> Sexp -> b) -> b -> [Sexp] -> b
walk proc acc src = foldl (\a x -> case x of
    List exprs -> walk proc (proc a x) exprs
    expr       -> proc a expr) acc src

