module SchemeDoc where

import Data.List (intercalate)

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
