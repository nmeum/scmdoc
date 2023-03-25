-- | Data types for the SchemeDoc library.
module SchemeDoc.Types (Documented, Sexp (..), Walk (..), walk)
where

import Data.Complex
import qualified Data.Text as T

-- | A documented S-expression, i.e. an S-expression which is preceeded
-- by a 'DocComment`.
type Documented = (T.Text, Sexp)

-- | Algebraic data type representing Scheme S-expressions.
data Sexp
    = Str T.Text -- "foo"
    | Id T.Text -- foo
    | Symbol T.Text -- 'foo
    | Char Char -- #\f
    | Boolean Bool -- #t
    | List [Sexp] -- ["foo" "bar"]
    | Number Integer
    | Float Double
    | Complex (Complex Double)
    | Rational Rational
    | DocComment T.Text
    deriving (Eq)

instance Show Sexp where
    show (Str s) = "\"" ++ T.unpack s ++ "\"" -- TODO: Perform escaping
    show (Id i) = T.unpack i
    show (Symbol s) = "'" ++ T.unpack s
    show (Char c) = "#\\" ++ [c]
    show (Boolean b) = if b then "#t" else "#f"
    show (List a) = "(" ++ unwords (map show a) ++ ")"
    show (Number n) = show n
    show (Float n) = show n
    show (Complex n) = show n
    show (Rational n) = show n
    show (DocComment c) = ";;> " ++ T.unpack c

-- | Type used for the return value of the closure passed to 'walk'.
-- The type is used to indicate whether 'walk' should recurse deeper
-- into a 'List' S-expression.
data Walk a = Recur a | Rise a

getValue :: Walk a -> a
getValue (Recur v) = v
getValue (Rise v) = v

-- | Traverse a Scheme source, i.e. a list of S-expressions.
--
-- For `List`s, the function used for traversal will first
-- be passed the `List` expression itself and then each
-- element of the list from left to right. If `proc` returns
-- 'False' as a first tuple element for a list, then the `List`
-- wont't be iterated over.
walk :: (b -> Sexp -> Walk b) -> b -> [Sexp] -> b
walk proc =
    foldl
        ( \a x -> case x of
            List exprs -> case proc a x of
                Recur r -> walk proc r exprs
                Rise r -> r
            expr -> getValue $ proc a expr
        )
