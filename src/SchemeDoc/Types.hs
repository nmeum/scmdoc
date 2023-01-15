module SchemeDoc.Types where

import Data.List (intercalate)
import Data.Text hiding (foldl, map, intercalate)
import Data.Complex

-- A documented S-expression
type Documented = (Text, Maybe Sexp)

-- Algebraic data type representing Scheme S-Expressions.
data Sexp = Str        Text    -- "foo"
          | Id         Text    -- foo
          | Symbol     Text    -- 'foo
          | Char       Char    -- #\f
          | Boolean    Bool    -- #t
          | List       [Sexp]  -- ["foo" "bar"]
          | Number     Integer
          | Float      Double
          | Complex    (Complex Double)
          | Rational   Rational
          | DocComment Text
    deriving (Eq)

instance Show Sexp where
    show (Str s) = "\"" ++ (unpack s) ++ "\"" -- TODO: Perform escaping
    show (Id i) = unpack i
    show (Symbol s) = "'" ++ (unpack s)
    show (Char c) = "#\\" ++ [c]
    show (Boolean b) = if b then "#t" else "#f"
    show (List a) = "(" ++ (intercalate " " (map show a)) ++ ")"
    show (Number n) = show n
    show (Float n) = show n
    show (Complex n) = show n
    show (Rational n) = show n
    show (DocComment c) = ";;> " ++ (unpack c)

-- Traverse a Scheme source, i.e. a list of S-expressions.
walk :: (b -> Sexp -> b) -> b -> [Sexp] -> b
walk proc acc src = foldl (\a x -> case x of
    List exprs -> walk proc (proc a x) exprs
    expr       -> proc a expr) acc src
