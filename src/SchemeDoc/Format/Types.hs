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
    fmt :: a -> FormatS

-- The default formatter for S-expression this is only intended to
-- be used as a catch-all fallback formatter.
instance Formatable Sexp where
    fmt expr = (\comment -> [ Heading H2 "Expression"
                              , Paragraph comment
                              , CodeBlock $ show expr ])

-- Type to convert an S-expression into a Formatted value.
type Formatter = Sexp -> Maybe FormatS
