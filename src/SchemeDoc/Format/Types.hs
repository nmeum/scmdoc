module SchemeDoc.Format.Types
    (FormatS, Formatable(..), Formatter)
where

import SchemeDoc.Types
import SchemeDoc.Output

-- A function for formatting an S-expressions.
-- Receives the documentation comment for the expression as an input.
type FormatS = (String -> [Block String])

-- Type class to convert a given type to a formatted S-expression.
class Formatable a where
    sid :: a -> String
    fmt :: a -> FormatS

-- Type to convert an S-expression into a unique id and a formatter.
type Formatter = Sexp -> Maybe (String, FormatS)
