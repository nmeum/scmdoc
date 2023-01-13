module SchemeDoc.Format.Types
    (FormatF, Format(..), Formatable(..), Formatter)
where

import SchemeDoc.Types
import Text.Blaze.Html

-- A function for formatting an S-expressions.
-- Receives the documentation comment for the expression as an input.
type FormatF = (String -> Html)

-- A Scheme expression prepared for formatting.
data Format = Format { fmtId   :: String
                     , fmtFunc :: FormatF }

-- Type class to convert a given type to a formatted S-expression.
class Formatable a where
    fmt :: a -> Format

-- Type to convert an S-expression into a unique id and a formatter.
type Formatter = Sexp -> Maybe Format
